;;; hledger-documents.el --- Manage document links in hledger entries -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Richard
;; Keywords: convenience, files, hledger
;; Version: 0.1.0

;;; Commentary:

;; This package provides functions to add document links (receipts, statements,
;; invoices) as tags to hledger journal entries.
;;
;; The documents are expected to be organized in a year-based directory structure:
;; <base-dir>/<year>/<files>
;;
;; Usage:
;;   M-x hledger-add-receipt
;;   M-x hledger-add-statement
;;   M-x hledger-add-document

;;; Code:

(require 'hledger-mode nil t)

;;; Customization

(defgroup hledger-documents nil
  "Document management for hledger."
  :group 'hledger
  :prefix "hledger-documents-")

(defcustom hledger-documents-base-directory
  (expand-file-name "~/Documents/hledger")
  "Base directory for hledger documents.
Documents should be organized in subdirectories by year."
  :type 'directory
  :group 'hledger-documents)

(defcustom hledger-documents-use-relative-paths nil
  "Whether to use relative paths in tags.
If t, store paths relative to `hledger-documents-base-directory'.
If nil, store absolute paths."
  :type 'boolean
  :group 'hledger-documents)

(defcustom hledger-documents-tag-format "%s: %s"
  "Format string for document tags.
First %s is the tag name (receipt, statement, etc.).
Second %s is the file path."
  :type 'string
  :group 'hledger-documents)

(defcustom hledger-documents-default-tag-types
  '("receipt" "statement" "invoice" "document" "attachment")
  "List of default document tag types."
  :type '(repeat string)
  :group 'hledger-documents)

;;; Internal functions

(defun hledger-documents--get-transaction-date ()
  "Get the date of the current hledger transaction.
Returns the date as a string in YYYY-MM-DD format, or nil if not found."
  (save-excursion
    (beginning-of-line)
    ;; Search backward for transaction line (starts with date)
    (when (re-search-backward "^[0-9]\\{4\\}[-/][0-9]\\{1,2\\}[-/][0-9]\\{1,2\\}" nil t)
      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (when (string-match "^\\([0-9]\\{4\\}\\)[-/]\\([0-9]\\{1,2\\}\\)[-/]\\([0-9]\\{1,2\\}\\)" line)
          (match-string 1 line))))))

(defun hledger-documents--get-year-from-date (date-string)
  "Extract year from DATE-STRING (YYYY-MM-DD format).
Returns the year as a string."
  (when (and date-string
             (string-match "^\\([0-9]\\{4\\}\\)" date-string))
    (match-string 1 date-string)))

(defun hledger-documents--get-transaction-year ()
  "Get the year of the current hledger transaction.
Returns the year as a string, or nil if not found."
  (let ((date (hledger-documents--get-transaction-date)))
    (when date
      (hledger-documents--get-year-from-date date))))

(defun hledger-documents--get-year-directory ()
  "Get the directory for the current transaction's year.
Returns the directory path, creating it if it doesn't exist."
  (let* ((year (hledger-documents--get-transaction-year))
         (year-dir (if year
                       (expand-file-name year hledger-documents-base-directory)
                     hledger-documents-base-directory)))
    (unless (file-directory-p year-dir)
      (when (y-or-n-p (format "Directory %s doesn't exist. Create it? " year-dir))
        (make-directory year-dir t)))
    year-dir))

(defun hledger-documents--make-relative-path (file-path)
  "Make FILE-PATH relative to `hledger-documents-base-directory'."
  (if (and hledger-documents-use-relative-paths
           (string-prefix-p hledger-documents-base-directory file-path))
      (file-relative-name file-path hledger-documents-base-directory)
    file-path))

(defun hledger-documents--find-tag-insertion-point ()
  "Find the appropriate point to insert a tag in the current transaction.
Returns the position, or nil if not in a transaction."
  (save-excursion
    (beginning-of-line)
    ;; Search backward for the transaction header
    (when (re-search-backward "^[0-9]\\{4\\}[-/][0-9]\\{1,2\\}[-/][0-9]\\{1,2\\}" nil t)
      (forward-line 1)
      ;; Skip existing comment lines that start with whitespace and ;
      (while (and (not (eobp))
                  (looking-at "^[[:space:]]+;"))
        (forward-line 1))
      ;; We're now at the first posting or after all comments
      ;; Insert before the first posting
      (beginning-of-line)
      (point))))

(defun hledger-documents--insert-tag (tag-type file-path)
  "Insert a document tag of TAG-TYPE with FILE-PATH.
Inserts the tag as a comment line in the current transaction."
  (let* ((relative-path (hledger-documents--make-relative-path file-path))
         (tag-line (format hledger-documents-tag-format tag-type relative-path))
         (insertion-point (hledger-documents--find-tag-insertion-point)))
    (if insertion-point
        (progn
          (goto-char insertion-point)
          (insert (format "    ; %s\n" tag-line))
          (message "Added %s tag: %s" tag-type relative-path))
      (error "Not in a hledger transaction"))))

;;; Interactive functions

;;;###autoload
(defun hledger-add-document (tag-type)
  "Add a document link with TAG-TYPE to the current hledger transaction.
Prompts for a file in the year-appropriate subdirectory."
  (interactive
   (list (completing-read "Tag type: "
                          hledger-documents-default-tag-types
                          nil nil "document")))
  (let* ((year-dir (hledger-documents--get-year-directory))
         (file (read-file-name
                (format "Select %s file: " tag-type)
                year-dir
                nil
                t)))
    (when file
      (hledger-documents--insert-tag tag-type file))))

;;;###autoload
(defun hledger-add-receipt ()
  "Add a receipt link to the current hledger transaction.
Prompts for a file in the year-appropriate subdirectory."
  (interactive)
  (let* ((year-dir (hledger-documents--get-year-directory))
         (file (read-file-name "Select receipt file: "
                               year-dir
                               nil
                               t)))
    (when file
      (hledger-documents--insert-tag "receipt" file))))

;;;###autoload
(defun hledger-add-statement ()
  "Add a statement link to the current hledger transaction.
Prompts for a file in the year-appropriate subdirectory."
  (interactive)
  (let* ((year-dir (hledger-documents--get-year-directory))
         (file (read-file-name "Select statement file: "
                               year-dir
                               nil
                               t)))
    (when file
      (hledger-documents--insert-tag "statement" file))))

;;;###autoload
(defun hledger-add-invoice ()
  "Add an invoice link to the current hledger transaction.
Prompts for a file in the year-appropriate subdirectory."
  (interactive)
  (let* ((year-dir (hledger-documents--get-year-directory))
         (file (read-file-name "Select invoice file: "
                               year-dir
                               nil
                               t)))
    (when file
      (hledger-documents--insert-tag "invoice" file))))

;;;###autoload
(defun hledger-open-document-at-point ()
  "Open the document referenced by the tag at point.
Works with both absolute and relative paths."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))
      ;; Match tag format: ; tagname: path
      (if (string-match ";[[:space:]]*\\([^:]+\\):[[:space:]]*\\(.+\\)$" line)
          (let* ((path (string-trim (match-string 2 line)))
                 (full-path (if (file-name-absolute-p path)
                               path
                             (expand-file-name path hledger-documents-base-directory))))
            (if (file-exists-p full-path)
                (find-file full-path)
              (error "File not found: %s" full-path)))
        (error "No document tag found on current line")))))

;;;###autoload
(defun hledger-open-document-externally ()
  "Open the document referenced by the tag at point with external viewer.
Uses `xdg-open' on Linux, `open' on macOS."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))
      (if (string-match ";[[:space:]]*\\([^:]+\\):[[:space:]]*\\(.+\\)$" line)
          (let* ((path (string-trim (match-string 2 line)))
                 (full-path (if (file-name-absolute-p path)
                               path
                             (expand-file-name path hledger-documents-base-directory)))
                 (opener (cond
                         ((eq system-type 'darwin) "open")
                         ((eq system-type 'windows-nt) "start")
                         (t "xdg-open"))))
            (if (file-exists-p full-path)
                (progn
                  (start-process "open-document" nil opener full-path)
                  (message "Opened: %s" full-path))
              (error "File not found: %s" full-path)))
        (error "No document tag found on current line")))))

;;;###autoload
(defun hledger-list-documents ()
  "List all documents in the current transaction.
Opens a buffer showing all document tags."
  (interactive)
  (save-excursion
    (let ((docs '())
          (start-point (save-excursion
                        (re-search-backward "^[0-9]\\{4\\}[-/][0-9]\\{1,2\\}[-/][0-9]\\{1,2\\}" nil t))))
      (when start-point
        (goto-char start-point)
        (while (and (not (eobp))
                   (or (looking-at "^[0-9]\\{4\\}")
                       (looking-at "^[[:space:]]")))
          (let ((line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
            (when (string-match ";[[:space:]]*\\([^:]+\\):[[:space:]]*\\(.+\\)$" line)
              (push (cons (match-string 1 line) (match-string 2 line)) docs)))
          (forward-line 1)))
      (if docs
          (let ((buf (get-buffer-create "*hledger-documents*")))
            (with-current-buffer buf
              (erase-buffer)
              (insert "Documents in current transaction:\n\n")
              (dolist (doc (reverse docs))
                (insert (format "%-15s: %s\n" (car doc) (cdr doc))))
              (goto-char (point-min)))
            (display-buffer buf))
        (message "No documents found in current transaction")))))

;;; Keybindings

(defvar hledger-documents-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c d r") 'hledger-add-receipt)
    (define-key map (kbd "C-c d s") 'hledger-add-statement)
    (define-key map (kbd "C-c d i") 'hledger-add-invoice)
    (define-key map (kbd "C-c d d") 'hledger-add-document)
    (define-key map (kbd "C-c d o") 'hledger-open-document-at-point)
    (define-key map (kbd "C-c d O") 'hledger-open-document-externally)
    (define-key map (kbd "C-c d l") 'hledger-list-documents)
    map)
  "Keymap for hledger-documents commands.")

;;;###autoload
(define-minor-mode hledger-documents-mode
  "Minor mode for managing documents in hledger journal files."
  :lighter " hldoc"
  :keymap hledger-documents-mode-map)

;;;###autoload
(defun hledger-documents-setup ()
  "Setup hledger-documents for hledger-mode buffers."
  (when (derived-mode-p 'hledger-mode)
    (hledger-documents-mode 1)))

;; Auto-enable in hledger-mode
;;;###autoload
(add-hook 'hledger-mode-hook 'hledger-documents-setup)

(provide 'hledger-documents)
;;; hledger-documents.el ends here
