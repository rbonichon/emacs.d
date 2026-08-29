;;; ox-whatsapp.el --- Org export backend for WhatsApp -*- lexical-binding: t; -*-

;; WhatsApp understands *bold*, _italic_, ~strike~, `code`, ```blocks```,
;; "> " quotes and "- "/"1. " lists.  It has no headings, no links markup and
;; no tables, and it re-wraps text itself -- so paragraphs must be UNFILLED,
;; one logical paragraph per line.

(require 'ox)
(require 'ox-ascii)

(defconst rb-whatsapp--br ""
  "Sentinel standing in for a hard line break while paragraphs are unfilled.")

(defun rb-whatsapp--unfill (s)
  "Collapse the hard newlines of S, preserving explicit line breaks."
  (when s
    (thread-last s
      (replace-regexp-in-string "[ \t]*\n[ \t]*" " ")
      (replace-regexp-in-string "  +" " ")
      (string-trim))))

(defun rb-whatsapp--restore-breaks (s)
  (replace-regexp-in-string (regexp-quote rb-whatsapp--br) "\n" s))

;;; Inline markup

(defun rb-whatsapp-bold          (_b contents _i) (format "*%s*" contents))
(defun rb-whatsapp-italic        (_i contents _n) (format "_%s_" contents))
(defun rb-whatsapp-underline     (_u contents _i) (format "_%s_" contents)) ; no underline
(defun rb-whatsapp-strike        (_s contents _i) (format "~%s~" contents))
(defun rb-whatsapp-code          (code _c _i) (format "`%s`" (org-element-property :value code)))
(defun rb-whatsapp-line-break    (&rest _) rb-whatsapp--br)

;;; Blocks

(defun rb-whatsapp-paragraph (_p contents _i)
  (rb-whatsapp--restore-breaks (rb-whatsapp--unfill contents)))

(defun rb-whatsapp-headline (headline contents info)
  "No headings in WhatsApp: a bold line, then the subtree."
  (let* ((title (rb-whatsapp--unfill (org-export-data (org-element-property :title headline) info)))
         (todo  (and (plist-get info :with-todo-keywords)
                     (org-element-property :todo-keyword headline)))
         (label (concat (when todo (concat (org-export-data todo info) " ")) title)))
    (concat "*" label "*\n\n" (or contents ""))))

(defun rb-whatsapp-section (_s contents _i) (or contents ""))
(defun rb-whatsapp-inner-template (contents _i) contents)   ; kills the TOC

(defun rb-whatsapp-template (contents info)
  (let ((title (rb-whatsapp--unfill (org-export-data (plist-get info :title) info))))
    (concat (when (org-string-nw-p title) (format "*%s*\n\n" title))
            contents)))

(defun rb-whatsapp--fence (text)
  (format "```\n%s```" (if (string-suffix-p "\n" text) text (concat text "\n"))))

(defun rb-whatsapp-src-block (block _c _i)
  (rb-whatsapp--fence (org-remove-indentation (org-element-property :value block))))

(defun rb-whatsapp-example-block (block _c _i)
  (rb-whatsapp--fence (org-remove-indentation (org-element-property :value block))))

(defun rb-whatsapp-fixed-width (fw _c _i)
  (rb-whatsapp--fence (org-remove-indentation (org-element-property :value fw))))

(defun rb-whatsapp-quote-block (_q contents _i)
  (mapconcat (lambda (l) (if (string-empty-p l) ">" (concat "> " l)))
             (split-string (string-trim contents) "\n")
             "\n"))

(defun rb-whatsapp-table (table contents info)
  "Tables do not exist in WhatsApp; a monospace block at least keeps the columns."
  (rb-whatsapp--fence (org-export-with-backend 'ascii table contents info)))

(defun rb-whatsapp-horizontal-rule (&rest _) "──────────")

(defun rb-whatsapp-item (item contents info)
  (let* ((parent (org-element-property :parent item))
         (type   (org-element-property :type parent))
         (struct (org-element-property :structure item))
         (bullet (if (not (eq type 'ordered)) "- "
                   (format "%d. "
                           (car (last (org-list-get-item-number
                                       (org-element-property :begin item) struct
                                       (org-list-prevs-alist struct)
                                       (org-list-parents-alist struct)))))))
         (body (string-trim (or contents ""))))
    (concat bullet
            (replace-regexp-in-string "\n" "\n  " body)
            "\n")))

(defun rb-whatsapp-plain-list (_l contents _i) (string-trim-right contents))

(defun rb-whatsapp-link (link desc info)
  (let ((type (org-element-property :type link))
        (raw  (org-element-property :raw-link link)))
    (cond
     ((not (member type '("http" "https" "mailto" "ftp"))) (or desc ""))
     ((or (null desc) (string= desc raw)) raw)
     (t (format "%s : %s" desc raw)))))

(defun rb-whatsapp-final (text _backend _info)
  "Trim trailing spaces and collapse runs of blank lines."
  (thread-last text
    (replace-regexp-in-string "[ \t]+$" "")
    (replace-regexp-in-string "\n\\{3,\\}" "\n\n")
    (string-trim)))

(org-export-define-derived-backend 'whatsapp 'ascii
  :filters-alist '((:filter-final-output . rb-whatsapp-final))
  :menu-entry
  '(?W "Export to WhatsApp"
       ((?W "As buffer"      rb-whatsapp-export-as-whatsapp)
        (?c "To clipboard"   rb-whatsapp-export-to-clipboard)))
  :translate-alist
  '((bold            . rb-whatsapp-bold)
    (italic          . rb-whatsapp-italic)
    (underline       . rb-whatsapp-underline)
    (strike-through  . rb-whatsapp-strike)
    (code            . rb-whatsapp-code)
    (verbatim        . rb-whatsapp-code)
    (line-break      . rb-whatsapp-line-break)
    (paragraph       . rb-whatsapp-paragraph)
    (headline        . rb-whatsapp-headline)
    (section         . rb-whatsapp-section)
    (inner-template  . rb-whatsapp-inner-template)
    (template        . rb-whatsapp-template)
    (src-block       . rb-whatsapp-src-block)
    (example-block   . rb-whatsapp-example-block)
    (fixed-width     . rb-whatsapp-fixed-width)
    (quote-block     . rb-whatsapp-quote-block)
    (table           . rb-whatsapp-table)
    (horizontal-rule . rb-whatsapp-horizontal-rule)
    (item            . rb-whatsapp-item)
    (plain-list      . rb-whatsapp-plain-list)
    (link            . rb-whatsapp-link)))

;;;###autoload
(defconst rb-whatsapp--options '(:with-toc nil :section-numbers nil)
  "Options forced on every WhatsApp export.")

(defun rb-whatsapp--plist (ext-plist)
  (org-combine-plists rb-whatsapp--options ext-plist))

;; `org-export-dispatch' applies the menu action to FOUR arguments --
;; async, subtreep, visible-only and body-only -- so every entry point
;; must accept them, plus the ext-plist the other ox backends take.
(defun rb-whatsapp-export-as-whatsapp (&optional async subtreep visible-only body-only ext-plist)
  "Export to a *Org WhatsApp Export* buffer."
  (interactive)
  (org-export-to-buffer 'whatsapp "*Org WhatsApp Export*"
    async subtreep visible-only body-only (rb-whatsapp--plist ext-plist)
    (lambda () (text-mode) (visual-line-mode 1))))

;;;###autoload
(defun rb-whatsapp-export-to-clipboard (&optional _async subtreep visible-only body-only ext-plist)
  "Export and put the result straight on the clipboard."
  (interactive)
  (let ((text (org-export-as 'whatsapp subtreep visible-only body-only
                             (rb-whatsapp--plist ext-plist))))
    (kill-new text)
    (message "WhatsApp export: %d characters on the clipboard" (length text))
    text))

(provide 'ox-whatsapp)
;;; ox-whatsapp.el ends here
