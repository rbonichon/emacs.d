;;; feed-discover.el --- Find RSS/Atom feeds on a web page  -*- lexical-binding: t; -*-

;; Author: Richard Bonichon <richard@delta.network>
;; Keywords: hypermedia, news, rss, atom

;;; Commentary:

;; Inspired by FeedBro: given a URL, find the RSS/Atom feeds the page
;; declares (via <link rel="alternate" type="application/rss+xml">
;; etc.), and optionally probe a list of conventional paths
;; (/feed, /rss.xml, /atom.xml...) when no feed link is declared.
;;
;; Results are shown in a dedicated buffer; key bindings let you browse
;; a feed, copy its URL, or append it to your elfeed-org file under a
;; heading of your choice.
;;
;; Entry point: M-x feed-discover.  In eww, the current page URL is
;; used as the default; elsewhere, a URL at point or in the kill ring
;; is offered before prompting.

;;; Code:

(require 'url)
(require 'url-parse)
(require 'dom)
(require 'seq)
(require 'subr-x)

(defgroup feed-discover nil
  "Discover RSS/Atom feeds on a web page."
  :group 'hypermedia
  :prefix "feed-discover-")

(defcustom feed-discover-common-paths
  '("/feed" "/feed/" "/feed.xml" "/rss" "/rss.xml" "/atom.xml"
    "/index.xml" "/feeds/all.atom.xml" "/feeds/posts/default")
  "Conventional feed paths probed when a page declares no feed link."
  :type '(repeat string))

(defcustom feed-discover-probe-common-paths t
  "When non-nil, probe `feed-discover-common-paths' if no <link> feed is found."
  :type 'boolean)

(defcustom feed-discover-elfeed-org-file
  (expand-file-name "~/documents/org/elfeed.org")
  "Path to the elfeed-org file used by `feed-discover-add-to-elfeed'."
  :type 'file)

(defcustom feed-discover-fetch-timeout 10
  "Timeout in seconds for HTTP requests."
  :type 'integer)

(defvar feed-discover--user-agent
  (format "Emacs/%s feed-discover" emacs-version)
  "User-Agent string sent with discovery requests.")

(defvar-local feed-discover--source-url nil
  "URL of the page whose feeds are listed in the current buffer.")

(defvar-local feed-discover--feeds nil
  "Discovered feeds as a list of plists in the current buffer.")


;;;; URL resolution

(declare-function eww-current-url "eww")

(defun feed-discover--default-url ()
  "Return a reasonable default URL based on context, or nil."
  (or (and (derived-mode-p 'eww-mode)
           (fboundp 'eww-current-url)
           (eww-current-url))
      (thing-at-point 'url t)
      (let ((kill (ignore-errors (current-kill 0 t))))
        (and (stringp kill)
             (string-match-p "\\`https?://" (string-trim kill))
             (string-trim kill)))))

(defun feed-discover--read-url ()
  "Prompt for a URL, defaulting to the contextual one."
  (let* ((default (feed-discover--default-url))
         (prompt (if default
                     (format "Discover feeds on URL (default %s): " default)
                   "Discover feeds on URL: ")))
    (let ((url (read-string prompt nil nil default)))
      (when (string-empty-p url)
        (user-error "No URL given"))
      url)))


;;;; HTTP

(defun feed-discover--with-fetched (url callback)
  "Fetch URL synchronously and call CALLBACK in the response buffer.
The buffer is killed after CALLBACK returns.  CALLBACK is called
with point at the start of the response body if a body is present;
otherwise with point at `point-min'.  Returns whatever CALLBACK
returns, or nil on failure."
  (let* ((url-request-extra-headers
          `(("User-Agent" . ,feed-discover--user-agent)))
         (buf (ignore-errors
                (url-retrieve-synchronously
                 url t t feed-discover-fetch-timeout))))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (search-forward "\n\n" nil t)
            (funcall callback))
        (kill-buffer buf)))))

(defun feed-discover--parse-html (url)
  "Fetch URL and return its parsed HTML DOM, or nil."
  (feed-discover--with-fetched
   url
   (lambda ()
     (libxml-parse-html-region (point) (point-max)))))


;;;; <link> extraction

(defun feed-discover--link-is-feed-p (link)
  "Non-nil if DOM LINK is an alternate feed link."
  (let ((type (dom-attr link 'type))
        (rel  (dom-attr link 'rel)))
    (and type
         (string-match-p "application/\\(rss\\|atom\\)\\+xml" type)
         (or (null rel)
             (string-match-p "\\balternate\\b" rel)))))

(defun feed-discover--absolutize (href base)
  "Resolve HREF relative to BASE."
  (cond
   ((null href) nil)
   ((string-match-p "\\`https?://" href) href)
   (t (url-expand-file-name href base))))

(defun feed-discover--from-dom (dom base-url)
  "Extract feed entries declared in DOM via <link>, as plists."
  (delq nil
        (mapcar
         (lambda (link)
           (when-let ((href (dom-attr link 'href)))
             (list :url (feed-discover--absolutize href base-url)
                   :title (or (dom-attr link 'title) "")
                   :type (dom-attr link 'type)
                   :source 'link)))
         (seq-filter #'feed-discover--link-is-feed-p
                     (dom-by-tag dom 'link)))))


;;;; Common-path probing

(defun feed-discover--root-of (url)
  "Return scheme://host[:port] for URL."
  (let* ((parsed (url-generic-parse-url url))
         (port (url-port-if-non-default parsed)))
    (concat (url-type parsed) "://"
            (url-host parsed)
            (if port (format ":%d" port) ""))))

(defun feed-discover--looks-like-feed-p ()
  "Return non-nil if point-min..point-max contains a feed-ish header."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     "<\\(\\?xml\\b\\|rss\\b\\|feed\\b\\|rdf:RDF\\b\\)"
     (min (point-max) (+ (point-min) 4096))
     t)))

(defun feed-discover--probe (url)
  "Probe URL.  Return URL if it responds with feed-looking content."
  (feed-discover--with-fetched
   url
   (lambda ()
     (when (feed-discover--looks-like-feed-p)
       url))))

(defun feed-discover--probe-common (base-url)
  "Probe `feed-discover-common-paths' under BASE-URL.
Return a list of plists for those that respond with feed-looking content."
  (let ((root (feed-discover--root-of base-url)))
    (delq nil
          (mapcar
           (lambda (path)
             (let* ((candidate (concat root path))
                    (hit (feed-discover--probe candidate)))
               (when hit
                 (list :url hit :title path
                       :type "probed" :source 'probe))))
           feed-discover-common-paths))))


;;;; Dedup

(defun feed-discover--dedup (feeds)
  "Remove duplicate feeds by URL, keeping the first occurrence."
  (let (seen out)
    (dolist (feed feeds)
      (let ((u (plist-get feed :url)))
        (unless (member u seen)
          (push u seen)
          (push feed out))))
    (nreverse out)))


;;;; UI

(defvar feed-discover-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'feed-discover-browse-feed)
    (define-key map (kbd "c")   #'feed-discover-copy-url)
    (define-key map (kbd "a")   #'feed-discover-add-to-elfeed)
    (define-key map (kbd "g")   #'feed-discover-refresh)
    (define-key map (kbd "n")   #'feed-discover-next)
    (define-key map (kbd "p")   #'feed-discover-prev)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `feed-discover-mode'.")

(define-derived-mode feed-discover-mode special-mode "FeedDiscover"
  "Major mode for the feed-discovery results buffer."
  (setq truncate-lines t)
  (setq-local cursor-in-non-selected-windows nil))

(defun feed-discover--render (url feeds)
  "Render FEEDS for URL in the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq feed-discover--source-url url
          feed-discover--feeds feeds)
    (insert (propertize (format "Feeds discovered on %s\n" url)
                        'face 'bold))
    (insert (propertize
             "RET browse  c copy  a add-to-elfeed  g refresh  n/p navigate  q quit\n\n"
             'face 'shadow))
    (if (null feeds)
        (insert (propertize "(no feeds found)\n" 'face 'italic))
      (dolist (feed feeds)
        (let ((beg (point))
              (src (plist-get feed :source))
              (title (plist-get feed :title))
              (type (plist-get feed :type)))
          (insert (format "  %-6s  %s\n"
                          (if (eq src 'link) "<link>" "probed")
                          (propertize (plist-get feed :url)
                                      'face 'link)))
          (when (and title (not (string-empty-p title)))
            (insert (format "          title: %s\n"
                            (propertize title 'face 'italic))))
          (when (and type (not (string-empty-p type))
                     (not (equal type "probed")))
            (insert (format "          type:  %s\n"
                            (propertize type 'face 'shadow))))
          (add-text-properties beg (point)
                               (list 'feed-discover-entry feed)))))
    (goto-char (point-min))
    (feed-discover-next)))

(defun feed-discover--entry-at-point ()
  "Return the feed plist at point or signal an error."
  (or (get-text-property (point) 'feed-discover-entry)
      (user-error "No feed at point")))

(defun feed-discover-next ()
  "Move point to the next feed entry."
  (interactive)
  (let ((start (point))
        (next (next-single-property-change
               (point) 'feed-discover-entry)))
    (when next
      (unless (get-text-property next 'feed-discover-entry)
        (setq next (next-single-property-change
                    next 'feed-discover-entry)))
      (when next (goto-char next)))
    (when (= (point) start)
      (goto-char (point-min))
      (let ((p (next-single-property-change
                (point) 'feed-discover-entry)))
        (when p (goto-char p))))))

(defun feed-discover-prev ()
  "Move point to the previous feed entry."
  (interactive)
  (let ((p (previous-single-property-change
            (point) 'feed-discover-entry)))
    (while (and p (not (get-text-property p 'feed-discover-entry)))
      (setq p (previous-single-property-change p 'feed-discover-entry)))
    (when p (goto-char p))))

(defun feed-discover-browse-feed ()
  "Open the feed URL at point with `eww'."
  (interactive)
  (eww (plist-get (feed-discover--entry-at-point) :url)))

(defun feed-discover-copy-url ()
  "Copy the URL of the feed at point to the kill ring."
  (interactive)
  (let ((url (plist-get (feed-discover--entry-at-point) :url)))
    (kill-new url)
    (message "Copied: %s" url)))

(defun feed-discover-refresh ()
  "Re-run discovery on the current source URL."
  (interactive)
  (let ((url feed-discover--source-url))
    (unless url (user-error "No source URL to refresh"))
    (feed-discover url)))


;;;; elfeed-org integration

(declare-function org-current-level "org")
(declare-function org-end-of-subtree "org")
(declare-function org-format-outline-path "org")
(declare-function org-get-outline-path "org")
(declare-function org-get-tags "org")
(declare-function org-map-entries "org")

(defun feed-discover--elfeed-org-headings ()
  "Return list of (LABEL . MARKER) for level-≥2 headings inside an :elfeed: tree."
  (with-current-buffer (find-file-noselect feed-discover-elfeed-org-file)
    (save-excursion
      (let (cats)
        (org-map-entries
         (lambda ()
           (let ((tags (org-get-tags)))
             (when (and (>= (org-current-level) 2)
                        (or (member "elfeed" tags)
                            ;; level-1 ancestor carries the tag
                            (member "elfeed" (org-get-tags nil t))))
               (push (cons (org-format-outline-path
                            (org-get-outline-path t) 80 nil " / ")
                           (point-marker))
                     cats)))))
        (nreverse cats)))))

(defun feed-discover-add-to-elfeed ()
  "Append the feed at point to `feed-discover-elfeed-org-file'.
Prompts for a target heading (must be inside an :elfeed: subtree),
an optional title, and optional extra tags."
  (interactive)
  (require 'org)
  (let* ((entry (feed-discover--entry-at-point))
         (url (plist-get entry :url))
         (cats (feed-discover--elfeed-org-headings))
         (_ (unless cats
              (user-error "No :elfeed: subtree headings found in %s"
                          feed-discover-elfeed-org-file)))
         (choice (completing-read "Add under heading: "
                                  (mapcar #'car cats) nil t))
         (marker (cdr (assoc choice cats)))
         (title (read-string "Title (RET = bare URL): "))
         (tags (read-string "Extra tags, colon-separated (RET = none): ")))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let ((parent-level (org-current-level)))
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (let* ((stars (make-string (1+ parent-level) ?*))
                 (body (if (string-empty-p title)
                           url
                         (format "[[%s][%s]]" url title)))
                 (tag-str (if (string-empty-p tags)
                              ""
                            (format " :%s:"
                                    (string-trim tags ":" ":")))))
            (insert (format "%s %s%s\n" stars body tag-str)))))
      (save-buffer))
    (message "Added feed under \"%s\"" choice)))


;;;; Entry point

;;;###autoload
(defun feed-discover (&optional url)
  "Discover RSS/Atom feeds on URL and show them in a buffer.
With no argument, prompt for URL, defaulting to the current
context (eww page, URL at point, or kill ring)."
  (interactive)
  (let* ((url (or url (feed-discover--read-url)))
         (_ (message "feed-discover: fetching %s..." url))
         (dom (feed-discover--parse-html url))
         (link-feeds (and dom (feed-discover--from-dom dom url)))
         (probed (when (and feed-discover-probe-common-paths
                            (null link-feeds))
                   (message "feed-discover: probing common paths...")
                   (feed-discover--probe-common url)))
         (feeds (feed-discover--dedup (append link-feeds probed))))
    (with-current-buffer (get-buffer-create "*feed-discover*")
      (feed-discover-mode)
      (feed-discover--render url feeds)
      (pop-to-buffer (current-buffer)))
    (message "feed-discover: %d feed(s) found on %s" (length feeds) url)))

(provide 'feed-discover)
;;; feed-discover.el ends here
