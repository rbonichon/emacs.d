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
(require 'seq)
(require 'subr-x)

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

;;; Code snippets
;;
;; A line
;;
;;   #+SNIPPET: :file lib/syntax/lexer.rs :title "Token"
;;
;; is replaced, before export, by a foldable source block (an HTML
;; <details> titled by :title, or the file name) holding the region of
;; that file delimited by the comments
;;
;;   // snippet: Token
;;   ...
;;   // end snippet
;;
;; in any comment syntax.  Without :title, the whole file is included.
;; Snippets start unfolded; :folded t starts them folded.
;;
;; The code comes from :repo, which is either
;; - a GitHub repository URL (https://github.com/OWNER/REPO), read from
;;   raw.githubusercontent.com; the block then links to the snippet's
;;   lines on GitHub;
;; - a local git repository path, read with `git show BRANCH:FILE' so
;;   uncommitted changes do not show; :branch worktree reads the file on
;;   disk instead, uncommitted changes included.
;; :repo and :branch default to the #+SNIPPET_URL: and #+SNIPPET_BRANCH:
;; keywords of the post.  :lang defaults to the file extension (see
;; `rb-website-snippet-languages').

(defcustom rb-website-snippet-languages
  '(("rs" . "rust") ("ml" . "ocaml") ("mli" . "ocaml") ("el" . "emacs-lisp")
    ("py" . "python") ("sh" . "shell"))
  "Source block language for each file extension; others use the extension."
  :type '(alist :key-type string :value-type string))

(defun rb-website--keyword (name)
  "Value of the #+NAME: keyword in the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward
             (format "^[ \t]*#\\+%s:[ \t]*\\(.*?\\)[ \t]*$" name) nil t)
        (match-string-no-properties 1)))))

(defun rb-website--git-show (dir branch file)
  "Contents of FILE at BRANCH in the git repository DIR.
BRANCH `worktree' reads the file on disk instead, uncommitted changes
included: handy while drafting, before the code is committed."
  (with-temp-buffer
    (if (equal branch "worktree")
        (let ((path (expand-file-name file dir)))
          (unless (file-readable-p path)
            (user-error "SNIPPET: cannot read %s" path))
          (insert-file-contents path))
      (unless (zerop (call-process "git" nil t nil "-C" (expand-file-name dir)
                                   "show" (format "%s:%s" branch file)))
        (user-error "SNIPPET: cannot read %s at %s in %s: %s"
                    file branch dir (string-trim (buffer-string)))))
    (buffer-string)))

(defun rb-website--github-repo (url)
  "OWNER/REPO of the GitHub repository URL."
  (if (string-match "\\`https?://github\\.com/\\([^/]+/[^/]+?\\)\\(?:\\.git\\)?/?\\'" url)
      (match-string 1 url)
    (user-error "SNIPPET: %s is not a GitHub repository URL (https://github.com/OWNER/REPO)" url)))

(defun rb-website--github-fetch (url branch file)
  "Contents of FILE at BRANCH in the GitHub repository URL."
  (when (equal branch "worktree")
    (user-error "SNIPPET: :branch worktree needs a local :repo, not a URL"))
  (let ((raw (format "https://raw.githubusercontent.com/%s/%s/%s"
                     (rb-website--github-repo url) branch file)))
    (with-temp-buffer
      (unless (zerop (call-process "curl" nil t nil "-fsSL" raw))
        (user-error "SNIPPET: cannot fetch %s: %s" raw (string-trim (buffer-string))))
      (buffer-string))))

(defun rb-website--github-link (url branch file first last)
  "GitHub URL of lines FIRST to LAST of FILE at BRANCH in repository URL."
  (format "https://github.com/%s/blob/%s/%s%s"
          (rb-website--github-repo url) branch file
          (if first (format "#L%d-L%d" first last) "")))

(defun rb-website--snippet-marker-p (line)
  "Non-nil when LINE is a `snippet: TITLE' or `end snippet' comment."
  (string-match-p "\\(snippet:\\|end snippet\\)" line))

(defun rb-website--snippet-region (code title file)
  "Region of CODE between the `snippet: TITLE' comment and its `end snippet'.
Return (BODY FIRST LAST): BODY without any marker line, and the 1-based
numbers of the first and last lines of the region in CODE (nil without
TITLE, where BODY is the whole CODE).  Snippets nest: each `snippet:'
comment needs its own `end snippet'."
  (let* ((lines (split-string code "\n"))
         (start (and title
                     (format "snippet:[ \t]*%s[ \t]*\\(\\*)\\|-->\\)?[ \t]*$"
                             (regexp-quote title))))
         (begin (and title
                     (seq-position lines nil
                                   (lambda (line _) (string-match-p start line)))))
         (depth 1)
         body)
    (when (and title (not begin))
      (user-error "SNIPPET: no `snippet: %s' comment in %s" title file))
    (if (not title)
        (setq body lines)
      (catch 'done
        (dolist (line (nthcdr (1+ begin) lines))
          (cond ((string-match-p "end snippet" line)
                 (when (zerop (setq depth (1- depth))) (throw 'done nil)))
                ((string-match-p "snippet:" line) (setq depth (1+ depth))))
          (push line body))
        (user-error "SNIPPET: no `end snippet' for `%s' in %s" title file))
      (setq body (nreverse body)))
    (list (mapconcat #'identity (seq-remove #'rb-website--snippet-marker-p body) "\n")
          (and title (+ begin 2))
          (and title (+ begin 1 (length body))))))

(defun rb-website--expand-snippets (&optional _backend)
  "Replace each #+SNIPPET: line of the buffer by its source block."
  (let ((post-repo (rb-website--keyword "SNIPPET_URL"))
        (default-branch (or (rb-website--keyword "SNIPPET_BRANCH") "HEAD"))
        (case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#\\+SNIPPET:[ \t]*\\(.*\\)$" nil t)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (args (car (read-from-string (format "(%s)" (match-string 1)))))
             (arg (lambda (key) (let ((v (plist-get args key)))
                                  (and v (format "%s" v)))))
             (file (or (funcall arg :file) (user-error "SNIPPET: missing :file")))
             (repo (or (funcall arg :repo) post-repo
                       (user-error "SNIPPET: no :repo and no #+SNIPPET_URL:")))
             (url (and (string-match-p "\\`https?://" repo) repo))
             (branch (or (funcall arg :branch) default-branch))
             (title (funcall arg :title))
             (ext (file-name-extension file))
             (lang (or (funcall arg :lang)
                       (cdr (assoc ext rb-website-snippet-languages))
                       ext))
             (code (if url
                       (rb-website--github-fetch url branch file)
                     (rb-website--git-show repo branch file)))
             (region (rb-website--snippet-region code title file))
             (body (string-trim-right (car region)))
             (link (and url (apply #'rb-website--github-link url branch file
                                   (cdr region)))))
        ;; Reading the code clobbers the match data: replace by position.
        (delete-region start end)
        (goto-char start)
        (insert (format "#+begin_export html\n<details class=\"snippet\"%s><summary>%s%s</summary>\n#+end_export\n"
                        (if (member (funcall arg :folded) '("t" "yes" "true")) "" " open")
                        (org-html-encode-plain-text (or title file))
                        (if link
                            (format " <a class=\"snippet-source\" href=\"%s\">source</a>"
                                    (org-html-encode-plain-text link))
                          ""))
                (format "#+begin_src %s\n%s\n#+end_src\n" lang
                        (org-escape-code-in-string body))
                "#+begin_export html\n</details>\n#+end_export")))))

(add-hook 'org-export-before-processing-functions #'rb-website--expand-snippets)

;;; Sidenotes
;;
;; Each footnote is exported twice: as the usual reference and entry in
;; the footnotes section, and as a Tufte sidenote next to the reference.
;; site.css shows the sidenotes when the screen leaves room for a margin,
;; and the footnotes section otherwise.

(defun rb-website--footnote-reference (reference contents info)
  "Org footnote REFERENCE, followed by its definition as a sidenote."
  (concat
   (org-html-footnote-reference reference contents info)
   (when (org-export-footnote-first-reference-p reference info)
     (let ((text (org-trim
                  (org-export-data
                   (org-export-get-footnote-definition reference info) info))))
       (format "<span class=\"sidenote-number\"></span><span class=\"sidenote\">%s</span>"
               ;; A sidenote sits inside a paragraph: drop the <p> wrappers.
               (replace-regexp-in-string "</?p\\(?: [^>]*\\)?>" "" text))))))

(defun rb-website--link (link contents info)
  "Org LINK, keeping site-absolute links ([[/teaching/]]) site-absolute.
Org exports them as file:///…, which resolves nowhere once published."
  (replace-regexp-in-string "\\(href\\|src\\)=\"file:///" "\\1=\"/"
                            (org-html-link link contents info)))

(org-export-define-derived-backend 'rb-html 'html
  :translate-alist '((footnote-reference . rb-website--footnote-reference)
                     (link . rb-website--link)))

(defun rb-website--publish-to-html (plist filename pub-dir)
  "Publish FILENAME to PUB-DIR with the `rb-html' backend."
  (org-publish-org-to 'rb-html filename ".html" plist pub-dir))

(defcustom rb-website-mathjax-scale 0.9
  "MathJax scale on the site.
MathJax's TeX font runs larger than et-book at the same size: 0.9 makes
an inline \\(P\\) as tall as the P of the text around it."
  :type 'number)

(defun rb-website--mathjax-options ()
  "`org-html-mathjax-options' with the scale set to `rb-website-mathjax-scale'."
  (cons (list 'scale rb-website-mathjax-scale)
        (assq-delete-all 'scale (copy-tree org-html-mathjax-options))))

(defun rb-website--html-head ()
  "Stylesheet links injected inside <head>.

Absolute paths so they resolve at the site root regardless of the page
depth (e.g. /teaching/2021-asi36.html)."
  (concat
   "<link rel=\"stylesheet\" href=\"/css/tufte.css\"/>\n"
   "<link rel=\"stylesheet\" href=\"/css/site.css\"/>\n"
   ;; Syntax highlighting happens in the browser, with highlight.js.
   "<link rel=\"stylesheet\" href=\"/css/hljs/github.min.css\""
   " media=\"(prefers-color-scheme: light)\"/>\n"
   "<link rel=\"stylesheet\" href=\"/css/hljs/github-dark.min.css\""
   " media=\"(prefers-color-scheme: dark)\"/>\n"
   "<script defer src=\"/js/highlight.min.js\"></script>\n"
   "<script defer src=\"/js/highlight-languages.min.js\"></script>\n"
   "<script defer src=\"/js/highlight-init.js\"></script>"))

;;; Bibliography
;;
;; data/biblio.yml is the single source of the publication list.
;; `rb-website-biblio' turns it into src-org/biblio.org (the Publications
;; page, also included by src-org/resume.org); the Typst CV in
;; ~/code/typst reads the YAML directly.  Publishing regenerates the org
;; file whenever the YAML is newer.

(defcustom rb-website-biblio-yaml "data/biblio.yml"
  "Bibliography source, relative to `rb-website-root'."
  :type 'string)

(defcustom rb-website-biblio-org "src-org/biblio.org"
  "Generated publication list, relative to `rb-website-root'."
  :type 'string)

;; Exactly 4 lines: src-org/resume.org includes biblio.org from line 5
;; (#+INCLUDE: "./biblio.org" :lines "5-").
(defconst rb-website--biblio-header
  "#+title: Publications

# GENERATED from data/biblio.yml by `rb-website-biblio': edit the YAML, not this file.
")

(declare-function yaml-parse-string "yaml")

(defun rb-website--biblio-clean (value)
  "VALUE as a string with whitespace runs collapsed."
  (string-join (split-string (format "%s" value)) " "))

(defun rb-website--biblio-initials (given)
  "Initials of GIVEN: \"Marie-Laure\" → \"M.-L.\", \"Todd R.\" → \"T. R.\"."
  (mapconcat (lambda (word)
               (mapconcat (lambda (part) (concat (substring part 0 1) "."))
                          (split-string word "-" t) "-"))
             (split-string (rb-website--biblio-clean given)) " "))

(defun rb-website--biblio-authors (ref)
  "Author list of REF, without a trailing period."
  (string-remove-suffix
   "."
   (mapconcat (lambda (a)
                (concat (rb-website--biblio-initials (alist-get 'given a)) " "
                        (rb-website--biblio-clean (alist-get 'family a))))
              (alist-get 'author ref) ", ")))

(defun rb-website--biblio-venue (ref)
  "Venue, year, award, series and pages of REF."
  (let-alist ref
    (concat
     (if (equal .type "article-journal")
         (concat "*" (rb-website--biblio-clean .venue) "*"
                 (when .volume (concat " " (rb-website--biblio-clean .volume)))
                 (when (and .volume .issue)
                   (format "(%s)" (rb-website--biblio-clean .issue))))
       (format "*%s %s*" (rb-website--biblio-clean .venue)
               (rb-website--biblio-clean .year)))
     (when .note (concat " — " (rb-website--biblio-clean .note)))
     (when .series (concat ", " (rb-website--biblio-clean .series)))
     (when .page
       (concat ", p. " (string-replace "-" "–" (rb-website--biblio-clean .page)))))))

(defun rb-website--biblio-links (ref)
  "pdf · slides · arxiv · doi links of REF."
  (let-alist ref
    (string-join
     (delq nil
           (list
            (when .paper
              (format "[[./papers/%s][pdf]]" (rb-website--biblio-clean .paper)))
            (when .slides
              (format "[[./slides/%s][slides]]" (rb-website--biblio-clean .slides)))
            (when .URL
              (format "[[%s][%s]]" (rb-website--biblio-clean .URL)
                      (if (string-match-p "arxiv\\.org" .URL) "arxiv" "link")))
            (when .DOI
              (format "[[https://doi.org/%s][doi]]"
                      (downcase (rb-website--biblio-clean .DOI))))))
     " · ")))

(defun rb-website--biblio-entry (ref)
  "Org list item for REF: title, authors, then venue and links, one per
line as in the Typst CV.  No filling: it would split links and emphasis
markers.  The venue line is wrapped in .pub-venue for site.css, with
spaces inside the span: org emphasis cannot touch an @@ snippet."
  (let ((links (rb-website--biblio-links ref)))
    (format "- /%s/ \\\\\n  %s \\\\\n  @@html:<span class=\"pub-venue\">@@ %s%s @@html:</span>@@"
            (rb-website--biblio-clean (alist-get 'title ref))
            (rb-website--biblio-authors ref)
            (rb-website--biblio-venue ref)
            (if (string-empty-p links) "" (concat " · " links)))))

(defun rb-website--biblio-year (ref)
  "Publication year of REF as a number."
  (let ((year (alist-get 'year ref)))
    (if (numberp year) year (string-to-number year))))

;;;###autoload
(defun rb-website-biblio (&optional force)
  "Generate `rb-website-biblio-org' from `rb-website-biblio-yaml'.
Does nothing when the org file is newer than the YAML, unless FORCE."
  (interactive "P")
  (require 'yaml)
  (let ((src (rb-website--path rb-website-biblio-yaml))
        (dst (rb-website--path rb-website-biblio-org)))
    (when (or force (file-newer-than-file-p src dst))
      (let* ((refs (alist-get 'references
                              (yaml-parse-string (rb-website--read rb-website-biblio-yaml)
                                                 :object-type 'alist
                                                 :sequence-type 'list)))
             ;; Stable sort: keeps the YAML order within a year.
             (refs (sort refs :key #'rb-website--biblio-year :reverse t))
             (years (seq-group-by #'rb-website--biblio-year refs)))
        (with-temp-file dst
          (insert rb-website--biblio-header)
          (dolist (year (sort (mapcar #'car years) #'>))
            (insert (format "\n* %d\n\n" year)
                    (mapconcat #'rb-website--biblio-entry
                               (alist-get year years) "\n\n")
                    "\n")))
        (message "Generated %s" dst)))))

;;;###autoload
(defun rb-website-publish (&optional force)
  "Publish the site to `rb-website-root'/docs.
With FORCE (prefix argument), republish all files regardless of timestamps."
  (interactive "P")
  (let ((org-publish-project-alist (rb-website--projects "rb" "docs" nil))
        ;; Plain code blocks: highlight.js colours them in the browser.
        (org-html-htmlize-output-type nil)
        (org-html-mathjax-options (rb-website--mathjax-options)))
    (rb-website-biblio)
    (org-publish-project "rb-all" force)))

;;;###autoload
(defun rb-website-preview (&optional force)
  "Publish the site and the drafts to `rb-website-root'/_preview.
Drafts land under /drafts/.  Nothing is written to docs/, so `make
publish' never picks them up.  With FORCE, republish all files."
  (interactive "P")
  (let ((org-publish-project-alist (rb-website--projects "rb-preview" "_preview" t))
        (org-html-htmlize-output-type nil)
        (org-html-mathjax-options (rb-website--mathjax-options)))
    (rb-website-biblio)
    (org-publish-project "rb-preview-all" force)))

(defun rb-website--projects (prefix pubdir drafts)
  "Return an `org-publish-project-alist' publishing into PUBDIR.
Project names start with PREFIX, so each output directory keeps its own
timestamp cache.  When DRAFTS is non-nil, also publish drafts/ to
PUBDIR/drafts."
  (let ((name (lambda (suffix) (concat prefix "-" suffix)))
        (out (rb-website--path pubdir))
        (pages `(:base-extension "org"
                 :recursive t
                 :publishing-function rb-website--publish-to-html
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
                 :auto-sitemap nil)))
    `((,(funcall name "pages")
       :base-directory ,(rb-website--path "src-org")
       :publishing-directory ,out
       ,@pages)
      ,@(when drafts
          `((,(funcall name "drafts")
             :base-directory ,(rb-website--path "drafts")
             :publishing-directory ,(expand-file-name "drafts" out)
             ,@pages)))
      (,(funcall name "css")
       :base-directory ,(rb-website--path "css")
       :base-extension
       "css\\|woff2?\\|ttf\\|otf\\|eot\\|svg"
       :publishing-directory ,(expand-file-name "css" out)
       :recursive t
       :publishing-function org-publish-attachment)
      (,(funcall name "static")
       :base-directory ,(rb-website--path "static")
       :base-extension
       "png\\|jpe?g\\|gif\\|svg\\|pdf\\|ico\\|webp\\|js"
       :publishing-directory ,out
       :recursive t
       :publishing-function org-publish-attachment)
      (,(funcall name "all")
       :components ,(mapcar name (if drafts
                                     '("pages" "drafts" "css" "static")
                                   '("pages" "css" "static")))))))

(provide 'rb-website)
;;; rb-website.el ends here
