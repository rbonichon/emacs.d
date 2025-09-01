;; Thin wrapper over autorandr command

(defun autorandr-timestamp ()
  (format-time-string
   "%Y-%m-%dT%H:%M:%S"))

(defun run-autorandr (args)
  "Call autorandr command with argument ARGS. Arguments must all be
strings."
  "Run CMD in buffer."
  (let* ((buffer-name "*autorandr*")
         (buf (get-buffer-create buffer-name))
         ;; derive name from command using the first word
         (procname "autorandr")
         (cmd "autorandr"))
    (with-current-buffer
        buf
      (erase-buffer)
      (insert
       (format
        "* Command \"%s\" started at %s *"
        (string-join
         (cons cmd args)
         " ")
        (autorandr-timestamp))))
    (eval
     (append
      `(start-process
        ,procname
        ,buffer-name
        ,cmd)
      args))))

(defun autorandr--configurations ()
  "Retrieve the list of configurations from autorandr.

This uses autorandr --list.
"
  (split-string (shell-command-to-string "autorandr --list") "\n" t)
  )

(defun autorandr--read-configuration ()
  (let ((collection (autorandr--configurations)))
    (completing-read
      "Autorandr configuration: "
      collection)))

(defun autorandr--apply-configuration (arg &optional c)
  (let* ((conf (or c
                   (autorandr--read-configuration)))
         (params `(,arg ,conf)))
    (run-autorandr params)
    ))

(defun autorandr (&optional c)
  "docstring"
  (interactive)
  (autorandr--apply-configuration "-l" c))


;; Keybindings

;; (global-set-key
;;  (kbd "C-c n s -")
;;  (lambda ()
;;    (interactive)
;;    (autorandr-dec 10)))

;; (global-set-key
;;  (kbd "C-c n s +")
;;  (lambda ()
;;    (interactive)
;;    (autorandr-inc 10)))

(provide 'autorandr)
