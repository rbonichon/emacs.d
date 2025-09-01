(require 'auth-source)
(require 'org)
(require 's)
(require 'plz)

;; A youtube url is of the form: https://www.youtube.com/watch?v=OmyHt0JM7HM
(defvar yt-url-prefix "https://www.youtube.com/watch?v="
  "Usual prefix of youtube videos")

(defvar yt-org-prefix "yt" "docstring")

(defvar yt-api-host "youtube.googleapis.com"
  "Auth-source host under which the YouTube Data API key is stored.")

(defun yt--api-key ()
  "Return the YouTube Data API key from auth-source.
Never inline the key here: store it in ~/.authinfo.gpg as
  machine youtube.googleapis.com login apikey password <KEY>"
  (or (auth-source-pick-first-password :host yt-api-host)
      (user-error "No YouTube API key in auth-source for host %s" yt-api-host)))

(org-link-set-parameters
 yt-org-prefix
 :face '(:weight bold
        	 :underline t
        	 :background "#eeee")
 :follow (lambda (path)
           (shell-command
            (format
             "xdg-open \"%s%s\""
             yt-url-prefix
             path)
            )))

(defun yt--api-make-uri (yt-id)
  "docstring"
  (format
   "https://www.googleapis.com/youtube/v3/videos?part=snippet&id=%s&key=%s"
   yt-id
   (yt--api-key)))

(defun yt-get-info (yt-id)
  "docstring"
  (plz 'get (yt--api-make-uri yt-id) :as #'json-read)
  )

(defun yt-paste-clipboard-as-org-link (&optional url)
  "Paste a freshly copied youtube url from the clipboard "
  (interactive)
  (let* ((yt-url
          (or url
              (substring-no-properties
               (x-get-clipboard))))
         (yt-id
          (s-chop-prefix
           yt-url-prefix
           yt-url))
         (link
          (format
           "%s:%s"
           yt-org-prefix
           yt-id))
         (descr (yt-get-info yt-id))
         (title
          (cdr (assoc 'title
                      (cdr (assoc 'snippet
                                  (elt
                                   (cdr (assoc 'items descr))
                                   0)))))))
    (message (format "%s" title))
    (when yt-id
      (org-insert-link
       nil
       link
       title))))

;; (yt-paste-clipboard-as-org-link "https://www.youtube.com/watch?v=rT1kEMqtC4U")
;; [[yt:rT1kEMqtC4U][FAKE WEIGHTS PRANK... | ANATOLY pretended to be a Beginner in a Gym #15]]


