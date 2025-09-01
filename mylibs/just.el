;;; just.el --- Get and run targets from justfile -*- lexical-binding: t; -*-
;; Copyright (C) 2026
;; Author: Richard Bonichon
;; Keywords: tools, convenience
;; Version: 0.1.0
;;; Commentary:
;; This package provides functions to parse and run targets from a justfile
;; at the project root.  

(defun just-get-targets ()
  "Get list of targets from justfile at project root.
Returns a list of target names found in the justfile.
If called interactively, displays the targets in the minibuffer."
  (interactive)
  (let* ((project-root (rb--find-project-root))
         (justfile (expand-file-name "justfile" project-root))
         (targets '()))
    (if (not (file-exists-p justfile))
        (progn
          (message "No justfile found at %s" project-root)
          nil)
      (with-temp-buffer
        (insert-file-contents justfile)
        (goto-char (point-min))
        ;; Match lines that define targets: "target-name:" or "target-name arg:"
        ;; Ignore lines starting with # (comments), @ (recipe prefix), or whitespace
        (while (re-search-forward "^\\([a-zA-Z0-9_-]+\\)\\(\\s-+[^:]*\\)?:" nil t)
          (let ((target (match-string-no-properties 1)))
            (unless (member target targets)
              (push target targets)))))
      (setq targets (nreverse targets))
      (when (called-interactively-p 'any)
        (message "Targets: %s" (string-join targets ", ")))
      targets)))

;;;###autoload
(defun just-run-target (target)
  "Run a just TARGET with completion.
Prompts for a target from the justfile at project root and runs it
using the compile command."
  (interactive
   (list (completing-read "Just target: " (just-get-targets) nil t)))
  (let ((default-directory (rb--find-project-root)))
    (compile (format "just %s" target))))

(provide 'just)
;;; just-targets.el ends here
