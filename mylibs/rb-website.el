;;; rb-website.el --- Publish rbonichon.github.io via org-publish  -*- lexical-binding: t; -*-

;; Author: Richard Bonichon
;; Keywords: hypermedia, org

;;; Commentary:

;; Publishes the rbonichon.github.io site from a tree of org sources in
;; `rb-website-root'/src-org to `rb-website-root'/docs (the GitHub Pages
;; output, kept as a git submodule pointing at the master branch).
;;
;; Layout:
;;   src-org/             ← org sources (canonical)
;;   templates/header.html, footer.html  ← preamble / postamble fragments
;;   css/, static/        ← copied as-is into docs/
;;   docs/                ← output (submodule)
;;
;; Entry point: M-x rb-website-publish (C-u for FORCE republish all).

;;; Code:

(require 'ox-publish)
(require 'ox-html)

(defgroup rb-website nil
  "Static site publishing for rbonichon.github.io."
  :group 'hypermedia
  :prefix "rb-website-")

(defcustom rb-website-root
  (expand-file-name "~/documents/rbonichon.github.io")
  "Root of the rbonichon.github.io working tree."
  :type 'directory)

(defun rb-website--path (relpath)
  "Absolute path under `rb-website-root'."
  (expand-file-name relpath rb-website-root))

(defun rb-website--read (relpath)
  "Read RELPATH under `rb-website-root' and return its contents as a string."
  (with-temp-buffer
    (insert-file-contents (rb-website--path relpath))
    (buffer-string)))

(defun rb-website--html-head ()
  "Stylesheet links injected inside <head>.

Absolute paths so they resolve at the site root regardless of the page
depth (e.g. /teaching/2021-asi36.html)."
  (concat
   "<link rel=\"stylesheet\" href=\"/css/tufte.css\"/>\n"
   "<link rel=\"stylesheet\" href=\"/css/site.css\"/>"))

;;;###autoload
(defun rb-website-publish (&optional force)
  "Publish the site to `rb-website-root'/docs.
With FORCE (prefix argument), republish all files regardless of timestamps."
  (interactive "P")
  (let ((org-publish-project-alist
         `(("rb-pages"
            :base-directory ,(rb-website--path "src-org")
            :base-extension "org"
            :publishing-directory ,(rb-website--path "docs")
            :recursive t
            :publishing-function org-html-publish-to-html
            :headline-levels 4
            :html-doctype "html5"
            :html-html5-fancy t
            :html-head-include-default-style nil
            :html-head-include-scripts nil
            :html-head ,(rb-website--html-head)
            :html-preamble ,(rb-website--read "templates/header.html")
            :html-postamble ,(rb-website--read "templates/footer.html")
            :html-home/up-format ""
            :with-toc nil
            :section-numbers nil
            :auto-sitemap nil)
           ("rb-css"
            :base-directory ,(rb-website--path "css")
            :base-extension
            "css\\|woff2?\\|ttf\\|otf\\|eot\\|svg"
            :publishing-directory ,(rb-website--path "docs/css")
            :recursive t
            :publishing-function org-publish-attachment)
           ("rb-static"
            :base-directory ,(rb-website--path "static")
            :base-extension
            "png\\|jpe?g\\|gif\\|svg\\|pdf\\|ico\\|webp"
            :publishing-directory ,(rb-website--path "docs")
            :recursive t
            :publishing-function org-publish-attachment)
           ("rb-all" :components ("rb-pages" "rb-css" "rb-static")))))
    (org-publish-project "rb-all" force)))

(provide 'rb-website)
;;; rb-website.el ends here
