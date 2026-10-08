;;; yt-elfeed.el --- YouTube channels/playlists in elfeed, audio as mp3 -*- lexical-binding: t; -*-

;; Author: Richard Bonichon

;;; Commentary:
;; Workflow:
;;   1. M-x yt-elfeed/add  — give a YouTube channel (/@handle, /channel/UC…,
;;      /c/…, /user/…), a playlist (…?list=PL…) or even a video URL.  The
;;      Atom feed is resolved, its title fetched, and a
;;        *** [[https://www.youtube.com/feeds/videos.xml?…][Title]]
;;      line is appended under the :yt: heading of elfeed.org (tag inherited
;;      by every entry).  Duplicates are refused.  If elfeed is loaded, the
;;      feed is fetched right away.
;;   2. In elfeed (search or show buffer), `A' downloads the audio of the
;;      entry at point — or of every entry in the region — as mp3, through
;;      yt-dlp.  Downloads run one at a time in the background; the log is in
;;      the *yt-elfeed* buffer.  Finished entries get the `mp3' tag.
;;
;; Ads: YouTube's own ads are never part of the downloaded stream.  Sponsor
;; reads, self-promotion and "like & subscribe" segments are cut out with
;; SponsorBlock (`yt-elfeed/sponsorblock-remove'): community-submitted
;; timestamps, so a very fresh video may not have any yet.
;;
;; To download a whole channel: filter elfeed on it (s +yt =Name), C-x h, A.
;;
;; Automatic downloads: tag a feed line of elfeed.org with :autoaudio:
;;   *** [[https://www.youtube.com/feeds/videos.xml?…][Name]]   :autoaudio:
;; Every *new* entry of that feed (seen by elfeed for the first time, published
;; less than `yt-elfeed/auto-max-age-days' ago, Shorts excluded) is queued —
;; but not before it is `yt-elfeed/auto-min-age-hours' old, so SponsorBlock
;; has had time to collect segments.  Younger entries are tagged
;; `audio-pending' and picked up later by the timer
;; (`yt-elfeed/auto-update-interval'), which also fetches the :autoaudio:
;; feeds in the background, without opening elfeed.
;;
;; Podcast feeds for AntennaPod: every channel directory of
;; `yt-elfeed/audio-directory' gets a feed.xml (RSS 2.0 + itunes tags, built
;; from the .meta.json yt-dlp writes next to each mp3), plus index.html and
;; podcasts.opml at the root.  `yt-elfeed/podcast-publish' writes them and
;; rsyncs the tree to `yt-elfeed/podcast-target', served over HTTP by the
;; NixOS module ~/nix-config/modules/services/podcast-server.nix at
;; `yt-elfeed/podcast-base-url'.  Runs by itself once the download queue is
;; empty.  Subscribe in AntennaPod with <base-url><channel>/feed.xml (the
;; index.html page lists them).
;;
;; Needs yt-dlp and ffmpeg on `exec-path' (both in ~/nix-config,
;; home/modules/emacs.nix, emacs runtime deps).

;;; Code:

(require 'org)
(require 'url)
(require 'url-parse)
(require 'url-util)
(require 'xml)
(require 'subr-x)
(require 'seq)
(eval-when-compile (require 'elfeed-db nil t))

(declare-function elfeed-search-selected "elfeed-search")
(declare-function elfeed-search-update-entry "elfeed-search")
(declare-function elfeed-entry-link "elfeed-db")
(declare-function elfeed-entry-title "elfeed-db")
(declare-function elfeed-tag "elfeed-db")
(declare-function elfeed-untag "elfeed-db")
(declare-function elfeed-update-feed "elfeed")
(declare-function elfeed-entry-tags "elfeed-db")
(declare-function elfeed-entry-date "elfeed-db")
;; Used by the expansion of `with-elfeed-db-visit'.
(declare-function elfeed-entry-feed "elfeed-db")
(declare-function elfeed-db-get-entry "elfeed-db")
(declare-function elfeed-db-ensure "elfeed-db")
(declare-function avl-tree-mapc "avl-tree")
(defvar elfeed-feeds)
(declare-function rmh-elfeed-org-process "elfeed-org")
(defvar elfeed-show-entry)
(defvar elfeed-search-mode-map)
(defvar elfeed-show-mode-map)
(defvar rmh-elfeed-org-files)
(defvar rmh-elfeed-org-tree-id)

(defgroup yt-elfeed nil
  "YouTube feeds in elfeed and audio downloads."
  :group 'comm)

(defcustom yt-elfeed/org-file (expand-file-name "~/documents/org/elfeed.org")
  "elfeed-org file holding the feeds."
  :type 'file)

(defcustom yt-elfeed/org-tag "yt"
  "Tag of the elfeed.org heading under which YouTube feeds are added."
  :type 'string)

(defcustom yt-elfeed/audio-directory (expand-file-name "~/media/music/podcasts/youtube/")
  "Root directory of downloaded audio; one sub-directory per channel."
  :type 'directory)

(defcustom yt-elfeed/audio-quality "5"
  "yt-dlp --audio-quality: 0 (best VBR) to 10 (worst), or a bitrate like 128K."
  :type 'string)

(defcustom yt-elfeed/sponsorblock-remove '("sponsor" "selfpromo" "interaction")
  "SponsorBlock categories cut from the audio.
Other categories: intro, outro, preview, music_offtopic, filler.
nil disables SponsorBlock."
  :type '(repeat string))

(defcustom yt-elfeed/download-archive
  (expand-file-name "~/media/music/podcasts/youtube/.yt-dlp-archive")
  "yt-dlp archive file: videos listed there are never downloaded twice."
  :type 'file)

(defcustom yt-elfeed/auto-tag 'autoaudio
  "Feed tag (in elfeed.org) whose new entries are downloaded automatically."
  :type 'symbol)

(defcustom yt-elfeed/auto-max-age-days 7
  "Automatic downloads skip entries published more than this many days ago.
Keeps a newly tagged or newly added feed from pulling its whole backlog."
  :type 'integer)

(defcustom yt-elfeed/auto-min-age-hours 24
  "Automatic downloads wait until the video is this many hours old.
Gives SponsorBlock users time to submit the sponsor segments."
  :type 'integer)

(defcustom yt-elfeed/auto-skip-shorts t
  "When non-nil, automatic downloads ignore YouTube Shorts."
  :type 'boolean)

(defcustom yt-elfeed/auto-update-interval 3600
  "Seconds between background fetches of the `yt-elfeed/auto-tag' feeds.
nil disables the timer (new entries are then only seen when elfeed updates).
Takes effect when this library is loaded."
  :type '(choice (const :tag "Off" nil) integer))

(defcustom yt-elfeed/podcast-base-url "http://192.168.50.79:8089/youtube/"
  "URL at which `yt-elfeed/podcast-target' is served (trailing slash).
tuxedo's LAN address for now: pin it with a DHCP reservation on the router.
Switch to musicbox once it is racked."
  :type 'string)

(defcustom yt-elfeed/podcast-target "/srv/podcasts/youtube/"
  "rsync destination of the audio tree: a local directory or HOST:/path/.
nil disables publishing."
  :type '(choice (const :tag "Off" nil) string))

(defconst yt-elfeed/feed-prefix "https://www.youtube.com/feeds/videos.xml?")

(defconst yt-elfeed/buffer "*yt-elfeed*")

;;;; Feed resolution

(defun yt-elfeed/--fetch (url)
  "Return the body of URL as a decoded string.
Sends the consent cookie so EU requests are not redirected to
consent.youtube.com (the RSS <link> is missing from that page)."
  (let* ((url-request-extra-headers
          '(("Cookie" . "SOCS=CAI; CONSENT=YES+")
            ("Accept-Language" . "en")))
         (url-user-agent "Mozilla/5.0 (X11; Linux x86_64)")
         (buf (url-retrieve-synchronously url t t 20)))
    (unless buf (user-error "yt-elfeed: no answer from %s" url))
    (with-current-buffer buf
      (unwind-protect
          (progn
            (goto-char (point-min))
            (unless (re-search-forward "^HTTP/[0-9.]+ 200" (line-end-position) t)
              (user-error "yt-elfeed: %s → %s" url
                          (buffer-substring (point-min) (line-end-position))))
            (re-search-forward "\r?\n\r?\n" nil t)
            (decode-coding-string
             (buffer-substring-no-properties (point) (point-max)) 'utf-8))
        (kill-buffer buf)))))

(defun yt-elfeed/--channel-id-via-yt-dlp (url)
  "Ask yt-dlp for the channel id of URL, or nil."
  (when (executable-find "yt-dlp")
    (with-temp-buffer
      (when (zerop (call-process "yt-dlp" nil '(t nil) nil
                                 "--skip-download" "--playlist-items" "1"
                                 "--print" "channel_id" url))
        (let ((id (string-trim (buffer-string))))
          (and (string-match-p "\\`UC[[:alnum:]_-]+\\'" id) id))))))

(defun yt-elfeed/feed-url (url)
  "Return the YouTube Atom feed URL for channel/playlist/video URL."
  (let* ((u (url-generic-parse-url (string-trim url)))
         (path+query (url-path-and-query u))
         (path (or (car path+query) ""))
         (query (and (cdr path+query) (url-parse-query-string (cdr path+query))))
         (list-id (cadr (assoc "list" query))))
    (cond
     ((string-prefix-p yt-elfeed/feed-prefix url) url)
     (list-id (concat yt-elfeed/feed-prefix "playlist_id=" list-id))
     ((string-match "\\`/channel/\\(UC[[:alnum:]_-]+\\)" path)
      (concat yt-elfeed/feed-prefix "channel_id=" (match-string 1 path)))
     (t
      (let ((html (ignore-errors (yt-elfeed/--fetch url))))
        (cond
         ((and html
               (string-match
                "application/rss\\+xml\"[^>]*href=\"\\([^\"]+\\)\"" html))
          (match-string 1 html))
         ((when-let ((id (yt-elfeed/--channel-id-via-yt-dlp url)))
            (concat yt-elfeed/feed-prefix "channel_id=" id)))
         (t (user-error "yt-elfeed: no feed found for %s" url))))))))

(defun yt-elfeed/feed-title (feed-url)
  "Return the <title> of FEED-URL (fetching it also validates the feed)."
  (let ((xml (yt-elfeed/--fetch feed-url)))
    (if (string-match "<title>\\([^<]*\\)</title>" xml)
        (xml-substitute-special (match-string 1 xml))
      (user-error "yt-elfeed: %s is not a YouTube feed" feed-url))))

;;;; elfeed.org

(defun yt-elfeed/--feed-key (feed-url)
  "The channel_id=… or playlist_id=… part of FEED-URL, used for dedup."
  (and (string-match "\\(\\(?:channel\\|playlist\\)_id=[[:alnum:]_-]+\\)" feed-url)
       (match-string 1 feed-url)))

(defun yt-elfeed/--goto-yt-heading ()
  "Move to the heading carrying `yt-elfeed/org-tag' as a local tag."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (outline-next-heading))
      (when (member yt-elfeed/org-tag (org-get-tags nil t))
        (setq found (point))))
    (or found
        (user-error "yt-elfeed: no :%s: heading in %s"
                    yt-elfeed/org-tag yt-elfeed/org-file))))

(defun yt-elfeed/--insert-feed (feed-url title)
  "Append FEED-URL as [[FEED-URL][TITLE]] under the :yt: heading.
Return nil if the feed was already there."
  (with-current-buffer (find-file-noselect yt-elfeed/org-file)
    (save-excursion
      (goto-char (point-min))
      (if (search-forward (yt-elfeed/--feed-key feed-url) nil t)
          nil
        (yt-elfeed/--goto-yt-heading)
        (let ((level (org-current-level)))
          (org-end-of-subtree t t)
          (skip-chars-backward " \t\n")
          (forward-line 1)
          (insert (format "\n%s [[%s][%s]]\n"
                          (make-string (1+ level) ?*)
                          feed-url
                          (string-replace "]" ")" (string-replace "[" "(" title)))))
        (save-buffer)
        t))))

(defun yt-elfeed/--youtube-url-default ()
  "A YouTube URL from point or the kill ring, if any."
  (seq-find (lambda (s) (and s (string-match-p "youtu\\(\\.be\\|be\\.com\\)" s)))
            (list (thing-at-point 'url t)
                  (ignore-errors (current-kill 0 t)))))

;;;###autoload
(defun yt-elfeed/add (url)
  "Add the YouTube channel or playlist at URL to elfeed.org (tag :yt:)."
  (interactive
   (let ((def (yt-elfeed/--youtube-url-default)))
     (list (read-string (if def (format "YouTube URL (%s): " def) "YouTube URL: ")
                        nil nil def))))
  (let* ((feed (yt-elfeed/feed-url url))
         (title (yt-elfeed/feed-title feed)))
    (if (not (yt-elfeed/--insert-feed feed title))
        (message "yt-elfeed: « %s » is already in %s" title
                 (file-name-nondirectory yt-elfeed/org-file))
      (when (and (featurep 'elfeed-org) (featurep 'elfeed))
        (ignore-errors
          (rmh-elfeed-org-process rmh-elfeed-org-files rmh-elfeed-org-tree-id)
          (elfeed-update-feed feed)))
      (message "yt-elfeed: added « %s » → %s" title feed))
    feed))

;;;; Audio download

(defvar yt-elfeed/--queue nil
  "Pending downloads, a list of elfeed entries.")

(defvar yt-elfeed/--running nil
  "The yt-dlp process currently running, if any.")

(defun yt-elfeed/--youtube-entry-p (entry)
  (string-match-p "youtube\\.com/\\(watch\\|shorts/\\)\\|youtu\\.be/"
                  (or (elfeed-entry-link entry) "")))

(defun yt-elfeed/--output-stem ()
  "yt-dlp output template, without extension."
  (expand-file-name "%(channel)s/%(upload_date>%Y-%m-%d)s - %(title)s"
                    yt-elfeed/audio-directory))

(defun yt-elfeed/--args (url)
  "yt-dlp arguments to fetch URL as mp3."
  (append
   (list "--extract-audio" "--audio-format" "mp3"
         "--audio-quality" yt-elfeed/audio-quality
         "--embed-metadata" "--embed-thumbnail"
         "--no-playlist" "--newline"
         "--download-archive" yt-elfeed/download-archive
         "--write-thumbnail" "--convert-thumbnails" "jpg"
         "--output" (concat (yt-elfeed/--output-stem) ".%(ext)s")
         "--output" (concat "thumbnail:" (yt-elfeed/--output-stem) ".%(ext)s")
         ;; Small per-episode metadata for the podcast feed.
         "--print-to-file"
         "after_move:%(.{id,title,description,webpage_url,duration,timestamp,upload_date,channel,channel_url})j"
         (concat (yt-elfeed/--output-stem) ".meta.json"))
   (when yt-elfeed/sponsorblock-remove
     (list "--sponsorblock-remove"
           (string-join yt-elfeed/sponsorblock-remove ",")))
   (list url)))

(defun yt-elfeed/--log (fmt &rest args)
  (with-current-buffer (get-buffer-create yt-elfeed/buffer)
    (goto-char (point-max))
    (insert (apply #'format fmt args) "\n")))

(defun yt-elfeed/--tools-missing ()
  "Names of the required executables absent from `exec-path'."
  (seq-remove #'executable-find '("yt-dlp" "ffmpeg")))

(defun yt-elfeed/--next ()
  "Start the next queued download, if idle."
  (when (and yt-elfeed/--queue (not (process-live-p yt-elfeed/--running)))
    (let* ((entry (pop yt-elfeed/--queue))
           (url (elfeed-entry-link entry)))
      (make-directory yt-elfeed/audio-directory t)
      (yt-elfeed/--log "\n▶ %s\n  %s" (elfeed-entry-title entry) url)
      (setq yt-elfeed/--running
            (make-process
             :name "yt-elfeed"
             :buffer (get-buffer-create yt-elfeed/buffer)
             :command (cons "yt-dlp" (yt-elfeed/--args url))
             :sentinel
             (lambda (proc _event)
               (unless (process-live-p proc)
                 (if (zerop (process-exit-status proc))
                     (progn
                       (elfeed-tag entry 'mp3)
                       (elfeed-untag entry 'unread)
                       (ignore-errors (elfeed-search-update-entry entry))
                       (yt-elfeed/--log "✔ done")
                       (message "yt-elfeed: mp3 ready — %s (%d queued)"
                                (elfeed-entry-title entry)
                                (length yt-elfeed/--queue)))
                   (yt-elfeed/--log "✘ yt-dlp exited %d" (process-exit-status proc))
                   (message "yt-elfeed: FAILED %s — see %s"
                            (elfeed-entry-title entry) yt-elfeed/buffer))
                 (setq yt-elfeed/--running nil)
                 (if yt-elfeed/--queue
                     (yt-elfeed/--next)
                   (when yt-elfeed/podcast-target
                     (yt-elfeed/podcast-publish))))))))))

;;;###autoload
(defun yt-elfeed/download-audio ()
  "Download as mp3 the YouTube entry at point, or every entry in the region."
  (interactive)
  (when-let ((missing (yt-elfeed/--tools-missing)))
    (user-error "yt-elfeed: %s not on exec-path (add to ~/nix-config, home/modules/emacs.nix)"
                (string-join missing ", ")))
  (let* ((entries (cond ((derived-mode-p 'elfeed-show-mode) (list elfeed-show-entry))
                        ((derived-mode-p 'elfeed-search-mode) (elfeed-search-selected))
                        (t (user-error "yt-elfeed: not in an elfeed buffer"))))
         (yt (seq-filter #'yt-elfeed/--youtube-entry-p entries)))
    (unless yt (user-error "yt-elfeed: no YouTube entry selected"))
    (setq yt-elfeed/--queue (append yt-elfeed/--queue yt))
    (message "yt-elfeed: %d video(s) queued → %s" (length yt) yt-elfeed/audio-directory)
    (yt-elfeed/--next)))

(defun yt-elfeed/cancel ()
  "Empty the download queue and kill the running download."
  (interactive)
  (setq yt-elfeed/--queue nil)
  (when (process-live-p yt-elfeed/--running)
    (kill-process yt-elfeed/--running))
  (message "yt-elfeed: queue cleared"))

;;;; Podcast feeds

(defun yt-elfeed/--xml (s)
  (xml-escape-string (or s "")))

(defun yt-elfeed/--rfc822 (time)
  (let ((system-time-locale "C"))
    (format-time-string "%a, %d %b %Y %H:%M:%S +0000" time t)))

(defun yt-elfeed/--channel-dirs ()
  "Sub-directories of `yt-elfeed/audio-directory' holding mp3 files."
  (when (file-directory-p yt-elfeed/audio-directory)
    (seq-filter (lambda (d) (and (file-directory-p d)
                                 (directory-files d nil "\\.mp3\\'" t)))
                (directory-files yt-elfeed/audio-directory t "\\`[^.]"))))

(defun yt-elfeed/--episode (mp3)
  "Plist describing MP3, from its .meta.json when present."
  (let* ((stem (file-name-sans-extension mp3))
         (meta-file (concat stem ".meta.json"))
         (meta (and (file-readable-p meta-file)
                    (ignore-errors
                      (with-temp-buffer
                        (insert-file-contents meta-file)
                        (json-parse-buffer :object-type 'plist :null-object nil)))))
         (base (file-name-nondirectory stem))
         (date (and (string-match "\\`\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) - " base)
                    (match-string 1 base)))
         (time (cond ((plist-get meta :timestamp) (seconds-to-time (plist-get meta :timestamp)))
                     (date (date-to-time (concat date " 12:00:00")))
                     (t (file-attribute-modification-time (file-attributes mp3))))))
    (list :file (file-name-nondirectory mp3)
          :title (or (plist-get meta :title)
                     (replace-regexp-in-string "\\`[0-9-]\\{10\\} - " "" base))
          :description (plist-get meta :description)
          :link (plist-get meta :webpage_url)
          :guid (or (plist-get meta :id) base)
          :duration (plist-get meta :duration)
          :channel-url (plist-get meta :channel_url)
          :image (let ((jpg (concat stem ".jpg")))
                   (and (file-exists-p jpg) (file-name-nondirectory jpg)))
          :time time
          :size (file-attribute-size (file-attributes mp3)))))

(defun yt-elfeed/--channel-url (dir)
  "Public URL of channel DIR (with trailing slash)."
  (concat yt-elfeed/podcast-base-url
          (url-hexify-string (file-name-nondirectory (directory-file-name dir)))
          "/"))

(defun yt-elfeed/--rss (dir)
  "RSS 2.0 document for the channel directory DIR."
  (let* ((name (file-name-nondirectory (directory-file-name dir)))
         (base (yt-elfeed/--channel-url dir))
         (eps (sort (mapcar #'yt-elfeed/--episode
                            (directory-files dir t "\\.mp3\\'"))
                    (lambda (a b) (time-less-p (plist-get b :time) (plist-get a :time)))))
         (latest (car eps))
         (image (seq-some (lambda (e) (plist-get e :image)) eps)))
    (concat
     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
     "<rss version=\"2.0\" xmlns:itunes=\"http://www.itunes.com/dtds/podcast-1.0.dtd\">\n"
     "<channel>\n"
     (format "<title>%s</title>\n" (yt-elfeed/--xml name))
     (format "<link>%s</link>\n"
             (yt-elfeed/--xml (or (plist-get latest :channel-url) base)))
     (format "<description>%s — YouTube, audio (yt-elfeed)</description>\n"
             (yt-elfeed/--xml name))
     "<language>fr</language>\n"
     (format "<itunes:author>%s</itunes:author>\n" (yt-elfeed/--xml name))
     (if image
         (format "<itunes:image href=\"%s%s\"/>\n" base (url-hexify-string image))
       "")
     (format "<lastBuildDate>%s</lastBuildDate>\n" (yt-elfeed/--rfc822 nil))
     (mapconcat
      (lambda (e)
        (concat
         "<item>\n"
         (format "<title>%s</title>\n" (yt-elfeed/--xml (plist-get e :title)))
         (format "<guid isPermaLink=\"false\">%s</guid>\n" (yt-elfeed/--xml (plist-get e :guid)))
         (format "<pubDate>%s</pubDate>\n" (yt-elfeed/--rfc822 (plist-get e :time)))
         (format "<enclosure url=\"%s%s\" length=\"%d\" type=\"audio/mpeg\"/>\n"
                 base (url-hexify-string (plist-get e :file)) (plist-get e :size))
         (if (plist-get e :link)
             (format "<link>%s</link>\n" (yt-elfeed/--xml (plist-get e :link)))
           "")
         (format "<description>%s</description>\n"
                 (yt-elfeed/--xml (or (plist-get e :description) "")))
         (if (plist-get e :duration)
             (format "<itunes:duration>%d</itunes:duration>\n" (plist-get e :duration))
           "")
         (if (plist-get e :image)
             (format "<itunes:image href=\"%s%s\"/>\n"
                     base (url-hexify-string (plist-get e :image)))
           "")
         "</item>\n"))
      eps "")
     "</channel>\n</rss>\n")))

(defun yt-elfeed/--write (file content)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil file nil 'silent)))

(defun yt-elfeed/podcast-write-feeds ()
  "Write feed.xml in every channel directory, plus index.html and podcasts.opml."
  (interactive)
  (let ((dirs (yt-elfeed/--channel-dirs)))
    (dolist (d dirs)
      (yt-elfeed/--write (expand-file-name "feed.xml" d) (yt-elfeed/--rss d)))
    (yt-elfeed/--write
     (expand-file-name "podcasts.opml" yt-elfeed/audio-directory)
     (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<opml version=\"2.0\">\n"
             "<head><title>YouTube (yt-elfeed)</title></head>\n<body>\n"
             (mapconcat (lambda (d)
                          (format "<outline type=\"rss\" text=\"%s\" xmlUrl=\"%sfeed.xml\"/>\n"
                                  (yt-elfeed/--xml (file-name-nondirectory d))
                                  (yt-elfeed/--channel-url d)))
                        dirs "")
             "</body>\n</opml>\n"))
    (yt-elfeed/--write
     (expand-file-name "index.html" yt-elfeed/audio-directory)
     (concat "<!doctype html><meta charset=\"utf-8\">"
             "<meta name=\"viewport\" content=\"width=device-width\">"
             "<title>Podcasts YouTube</title><h1>Podcasts YouTube</h1>"
             "<p>Appui long sur un lien → copier, puis AntennaPod → Ajouter un podcast → par URL.</p><ul>"
             (mapconcat (lambda (d)
                          (let ((u (concat (yt-elfeed/--channel-url d) "feed.xml")))
                            (format "<li><a href=\"%s\">%s</a><br><code>%s</code></li>"
                                    u (yt-elfeed/--xml (file-name-nondirectory d)) u)))
                        dirs "")
             "</ul><p><a href=\"podcasts.opml\">podcasts.opml</a></p>"))
    (when (called-interactively-p 'interactive)
      (message "yt-elfeed: %d podcast feed(s) written" (length dirs)))
    dirs))

;;;###autoload
(defun yt-elfeed/podcast-publish ()
  "Regenerate the podcast feeds and rsync them to `yt-elfeed/podcast-target'."
  (interactive)
  (unless yt-elfeed/podcast-target
    (user-error "yt-elfeed: `yt-elfeed/podcast-target' is nil"))
  (yt-elfeed/podcast-write-feeds)
  (yt-elfeed/--log "⇪ rsync → %s" yt-elfeed/podcast-target)
  (make-process
   :name "yt-elfeed-rsync"
   :buffer (get-buffer-create yt-elfeed/buffer)
   :command (list "rsync" "-rt" "--chmod=D755,F644"
                  "--exclude=*.meta.json" "--exclude=.*"
                  "--exclude=*.part" "--exclude=*.ytdl" "--exclude=*.webm"
                  (file-name-as-directory (expand-file-name yt-elfeed/audio-directory))
                  yt-elfeed/podcast-target)
   :sentinel (lambda (proc _event)
               (unless (process-live-p proc)
                 (if (zerop (process-exit-status proc))
                     (progn (yt-elfeed/--log "✔ published → %s" yt-elfeed/podcast-base-url)
                            (message "yt-elfeed: podcasts published → %sindex.html"
                                     yt-elfeed/podcast-base-url))
                   (yt-elfeed/--log "✘ rsync exited %d" (process-exit-status proc))
                   (message "yt-elfeed: podcast publish FAILED — see %s"
                            yt-elfeed/buffer))))))

;;;; Automatic downloads

(defun yt-elfeed/--auto-p (entry)
  "Non-nil if new ENTRY should be downloaded automatically."
  (and (memq yt-elfeed/auto-tag (elfeed-entry-tags entry))
       (not (memq 'mp3 (elfeed-entry-tags entry)))
       (yt-elfeed/--youtube-entry-p entry)
       (not (and yt-elfeed/auto-skip-shorts
                 (string-match-p "/shorts/" (elfeed-entry-link entry))))
       (< (yt-elfeed/--age entry) (* 86400 yt-elfeed/auto-max-age-days))))

(defun yt-elfeed/--age (entry)
  "Seconds since ENTRY was published."
  (- (float-time) (elfeed-entry-date entry)))

(defun yt-elfeed/--ripe-p (entry)
  (>= (yt-elfeed/--age entry) (* 3600 yt-elfeed/auto-min-age-hours)))

(defun yt-elfeed/--enqueue (entry)
  "Queue ENTRY for download, clearing its `audio-pending' tag."
  (if-let ((missing (yt-elfeed/--tools-missing)))
      (yt-elfeed/--log "✘ skipped %s: %s missing"
                       (elfeed-entry-title entry) (string-join missing ", "))
    (elfeed-untag entry 'audio-pending)
    (setq yt-elfeed/--queue (append yt-elfeed/--queue (list entry)))
    (yt-elfeed/--log "⏳ auto-queued %s" (elfeed-entry-title entry))
    ;; Out of `elfeed-db-add': start the process once the insertion is done.
    (run-at-time 0 nil #'yt-elfeed/--next)))

(defun yt-elfeed/--on-new-entry (entry)
  "`elfeed-new-entry-hook' function: queue ENTRY if its feed is :autoaudio:.
Entries younger than `yt-elfeed/auto-min-age-hours' are tagged
`audio-pending' instead, for `yt-elfeed/process-pending'."
  (when (yt-elfeed/--auto-p entry)
    (if (yt-elfeed/--ripe-p entry)
        (yt-elfeed/--enqueue entry)
      (elfeed-tag entry 'audio-pending)
      (yt-elfeed/--log "⏸ pending (< %dh) %s"
                       yt-elfeed/auto-min-age-hours (elfeed-entry-title entry)))))

(defun yt-elfeed/process-pending ()
  "Queue the `audio-pending' entries that are now old enough."
  (interactive)
  (require 'elfeed)
  (let ((horizon (* 86400 yt-elfeed/auto-max-age-days))
        (n 0))
    (with-elfeed-db-visit (entry _feed)
      ;; The index is sorted newest first: stop past the age window.
      (when (> (yt-elfeed/--age entry) horizon)
        (elfeed-db-return))
      (when (and (memq 'audio-pending (elfeed-entry-tags entry))
                 (yt-elfeed/--ripe-p entry)
                 (not (memq entry yt-elfeed/--queue)))
        (yt-elfeed/--enqueue entry)
        (setq n (1+ n))))
    (when (called-interactively-p 'interactive)
      (message "yt-elfeed: %d pending entr%s queued" n (if (= n 1) "y" "ies")))
    n))

(defun yt-elfeed/update-auto-feeds ()
  "Fetch the feeds tagged `yt-elfeed/auto-tag' in elfeed.org."
  (interactive)
  (require 'elfeed)
  (require 'elfeed-org)
  (let ((rmh-elfeed-org-files (list yt-elfeed/org-file)))
    (rmh-elfeed-org-process rmh-elfeed-org-files rmh-elfeed-org-tree-id))
  (dolist (feed elfeed-feeds)
    (when (and (consp feed) (memq yt-elfeed/auto-tag (cdr feed)))
      (elfeed-update-feed (car feed)))))

(defvar yt-elfeed/--timer nil)

(when yt-elfeed/auto-update-interval
  (when (timerp yt-elfeed/--timer) (cancel-timer yt-elfeed/--timer))
  (setq yt-elfeed/--timer
        (run-at-time 300 yt-elfeed/auto-update-interval
                     (lambda ()
                       (with-demoted-errors "yt-elfeed: %S"
                         (yt-elfeed/process-pending)
                         (yt-elfeed/update-auto-feeds))))))

(with-eval-after-load 'elfeed
  (add-hook 'elfeed-new-entry-hook #'yt-elfeed/--on-new-entry 90)
  (define-key elfeed-search-mode-map (kbd "A") #'yt-elfeed/download-audio)
  (define-key elfeed-show-mode-map (kbd "A") #'yt-elfeed/download-audio))

(provide 'yt-elfeed)
;;; yt-elfeed.el ends here
