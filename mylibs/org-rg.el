(require 'rg)

(defun org-rg-search (pattern)
  "Search for PATTERN in org files using the rg package.
Searches in `org-directory'."
  (interactive "sSearch pattern: ")
  (unless (boundp 'org-directory)
    (error "org-directory is not set"))
  (unless (file-directory-p org-directory)
    (error "Org directory does not exist: %s" org-directory))
  (rg pattern "*.org" org-directory))

(defun org-rg-search-current-word ()
  "Search for the word at point in org files."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (if word
        (org-rg-search word)
      (message "No word at point"))))

(defun org-rg-search-literal (pattern)
  "Search for literal PATTERN in org files (no regex)."
  (interactive "sLiteral pattern: ")
  (unless (boundp 'org-directory)
    (error "org-directory is not set"))
  (unless (file-directory-p org-directory)
    (error "Org directory does not exist: %s" org-directory))
  (rg pattern "*.org" org-directory '("--fixed-strings")))

(defun org-rg-search-tags (tag)
  "Search for org TAG in files."
  (interactive "sTag to search: ")
  (org-rg-search (format ":%s:" tag)))

(defun org-rg-search-headings (heading)
  "Search for org HEADING in files."
  (interactive "sHeading to search: ")
  (org-rg-search (format "^\\*+.*%s" heading)))

(defun org-rg-search-case-sensitive (pattern)
  "Search for PATTERN in org files with case sensitivity."
  (interactive "sCase-sensitive pattern: ")
  (unless (boundp 'org-directory)
    (error "org-directory is not set"))
  (unless (file-directory-p org-directory)
    (error "Org directory does not exist: %s" org-directory))
  (rg pattern "*.org" org-directory '("--case-sensitive")))

(defun org-rg-search-context (pattern &optional context-lines)
  "Search for PATTERN in org files with context lines.
CONTEXT-LINES defaults to 3 if not specified."
  (interactive "sSearch pattern: \nP")
  (let ((context (or context-lines 3)))
    (unless (boundp 'org-directory)
      (error "org-directory is not set"))
    (unless (file-directory-p org-directory)
      (error "Org directory does not exist: %s" org-directory))
    (rg pattern "*.org" org-directory (list (format "--context=%d" context)))))

;; Optional: Add keybindings
;; (global-set-key (kbd "C-c s o") 'org-rg-search)
;; (global-set-key (kbd "C-c s w") 'org-rg-search-current-word)
;; (global-set-key (kbd "C-c s l") 'org-rg-search-literal)
;; (global-set-key (kbd "C-c s t") 'org-rg-search-tags)
;; (global-set-key (kbd "C-c s h") 'org-rg-search-headings)
;; (global-set-key (kbd "C-c s c") 'org-rg-search-case-sensitive)
