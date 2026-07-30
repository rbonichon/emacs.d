;;; claude-code-multi.el --- Multiple claude-code instances (vterm-style C-u) -*- lexical-binding: t; -*-

;; Extends `claude-code' (stevemolitor/claude-code.el v0.1.0), which hardcodes a
;; single `*claude*' buffer, so that a universal argument launches a NEW,
;; independent instance in its own buffer — mirroring `C-u M-x vterm'.
;;
;;   M-x claude-code            → the default single `*claude*' (unchanged)
;;   C-u M-x claude-code        → a new instance, prompts for a name
;;   M-x claude-code-multi-new  → same, directly  (also `<prefix> n')
;;   M-x claude-code-multi-list → tabulated list: switch / rename / kill
;;                                instances  (also `<prefix> l')
;;
;; Each instance is its own eat terminal / Claude process → its own context.
;; Note: the upstream helpers (toggle/kill/send/switch) still target `*claude*';
;; extra instances are interacted with directly in their buffers (like vterm)
;; or via the list buffer above.

;;; Code:

(require 'claude-code)
(require 'tabulated-list)

(defun claude-code-multi--unique (bare)
  "Return BARE, or BARE<N>, such that buffer `*BARE*' is not already live."
  (let ((name bare) (i 1))
    (while (get-buffer (format "*%s*" name))
      (setq i (1+ i)
            name (format "%s<%d>" bare i)))
    name))

(defun claude-code-multi--auto-name ()
  "Return the next free auto instance label: \"1\", \"2\", …"
  (let ((i 1))
    (while (get-buffer (format "*claude:%d*" i))
      (setq i (1+ i)))
    (number-to-string i)))

;;;###autoload
(defun claude-code-multi-new (name)
  "Start a NEW Claude instance in buffer `*claude:NAME*' with its own context.
Runs in the current project root, like `claude-code'.  Called interactively,
prompt for NAME (empty = auto-numbered).  Coexists with the default `*claude*'.

Mirrors `claude-code--start', which is not reusable here because it hardcodes
the `*claude*' buffer name (claude-code.el v0.1.0)."
  (interactive
   (list (let ((n (read-string "Claude instance name (empty = auto): ")))
           (if (string-empty-p n) (claude-code-multi--auto-name) n))))
  (require 'eat)
  (let* ((bare (claude-code-multi--unique (format "claude:%s" name)))
         (dir (funcall #'project-root (project-current t)))
         (default-directory dir)
         (buffer (get-buffer-create (format "*%s*" bare))))
    (with-current-buffer buffer
      (cd dir)
      (setq-local eat-term-name claude-code-term-name)
      (let ((process-adaptive-read-buffering nil))
        (apply #'eat-make bare claude-code-program nil claude-code-program-switches))
      (setq-local eat-term-name claude-code-term-name)
      (claude-code--setup-repl-faces)
      (run-hooks 'claude-code-start-hook)
      (sleep-for claude-code-startup-delay)
      (display-buffer buffer))
    (pop-to-buffer buffer)
    buffer))

;; Repurpose the prefix arg of `claude-code' to mean "new instance" (vterm-style).
(define-advice claude-code (:around (orig &optional arg) claude-code-multi)
  "With a prefix ARG, launch a NEW independent Claude instance instead of
prompting for a directory into the single `*claude*' buffer."
  (if arg
      (call-interactively #'claude-code-multi-new)
    (funcall orig arg)))

;;;; Instance list (tabulated-list) — select / rename / kill

(defun claude-code-multi--instances ()
  "Return live Claude instance buffers.
An instance buffer's name starts with `*claude' and has a process."
  (seq-filter (lambda (b)
                (let ((n (buffer-name b)))
                  (and n (string-prefix-p "*claude" n) (get-buffer-process b))))
              (buffer-list)))

(defun claude-code-multi--list-entries ()
  "Build `tabulated-list-entries' from the live Claude instances."
  (mapcar (lambda (b)
            (let ((proc (get-buffer-process b)))
              (list b (vector (buffer-name b)
                              (if (process-live-p proc) "live" "dead")
                              (abbreviate-file-name
                               (buffer-local-value 'default-directory b))))))
          (claude-code-multi--instances)))

(defun claude-code-multi--tabulated-buffer ()
  "Return the instance buffer on the current line, or signal an error."
  (let ((b (tabulated-list-get-id)))
    (if (buffer-live-p b) b (user-error "No Claude instance on this line"))))

(defun claude-code-multi-list-switch ()
  "Switch to the Claude instance on the current line."
  (interactive)
  (pop-to-buffer (claude-code-multi--tabulated-buffer)))

(defun claude-code-multi-list-switch-other-window ()
  "Display the Claude instance on the current line in another window."
  (interactive)
  (switch-to-buffer-other-window (claude-code-multi--tabulated-buffer)))

(defun claude-code-multi-list-rename (new-name)
  "Rename the buffer of the Claude instance on the current line to NEW-NAME."
  (interactive
   (list (read-string "New buffer name: "
                      (buffer-name (claude-code-multi--tabulated-buffer)))))
  (with-current-buffer (claude-code-multi--tabulated-buffer)
    (rename-buffer new-name t))
  (revert-buffer))

(defun claude-code-multi-list-kill ()
  "Kill the Claude instance on the current line."
  (interactive)
  (let ((b (claude-code-multi--tabulated-buffer)))
    (when (yes-or-no-p (format "Kill %s? " (buffer-name b)))
      (kill-buffer b)
      (revert-buffer))))

(defvar-keymap claude-code-multi-list-mode-map
  :doc "Keymap for `claude-code-multi-list-mode'."
  "RET" #'claude-code-multi-list-switch
  "o"   #'claude-code-multi-list-switch-other-window
  "r"   #'claude-code-multi-list-rename
  "k"   #'claude-code-multi-list-kill
  "n"   #'claude-code-multi-new
  "g"   #'revert-buffer)

(defun claude-code-multi--update-header ()
  "Refresh the tab line to show the key hints.
The header line is left to `tabulated-list-mode' for the column titles."
  (setq-local tab-line-format
              " Claude instances — [RET switch] [o other window] [r rename] \
[k kill] [n new] [g refresh] [q quit]"))

(define-derived-mode claude-code-multi-list-mode tabulated-list-mode "Claude-List"
  "Major mode listing running Claude instances."
  (setq tabulated-list-format [("Buffer" 30 t) ("Status" 6 t) ("Directory" 50 t)]
        tabulated-list-sort-key '("Buffer" . nil)
        tabulated-list-entries #'claude-code-multi--list-entries)
  (claude-code-multi--update-header)
  (tabulated-list-init-header))

;;;###autoload
(defun claude-code-multi-list ()
  "Show a tabulated list of running Claude instances."
  (interactive)
  (let ((buf (get-buffer-create "*claude instances*")))
    (with-current-buffer buf
      (claude-code-multi-list-mode)
      (tabulated-list-print))
    (pop-to-buffer buf)))

;; Discoverable keys in the upstream command map.
(when (boundp 'claude-code-command-map)
  (define-key claude-code-command-map (kbd "n") #'claude-code-multi-new)
  (define-key claude-code-command-map (kbd "l") #'claude-code-multi-list))

(provide 'claude-code-multi)
;;; claude-code-multi.el ends here
