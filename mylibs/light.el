;; Thin wrapper over the light command 

(defun light-timestamp ()
  (format-time-string
   "%Y-%m-%dT%H:%M:%S"))

(defun light (args)
  "Call light command with argument ARGS. Arguments must all be
strings."
  "Run CMD in buffer."
  (let* ((buffer-name "*light*")
         (buf (get-buffer-create buffer-name))
         ;; derive name from command using the first word
         (procname "light")
         (cmd "light"))
    (with-current-buffer
        buf
      (erase-buffer)
      (insert
       (format
        "* Command \"%s\" started at %s *"
        (string-join
         (cons cmd args)
         " ")
        (light-timestamp))))
    (eval
     (append
      `(start-process
        ,procname
        ,buffer-name
        ,cmd)
      args))))

(defvar light-increments (list 5 10 20 50)
  "Increments for light command")

(defun light--read-increment ()
  (string-to-number
   (completing-read
    "Light increment: "
    light-increments)))

(defun light--apply-increment (arg &optional n)
  (let ((n (or n (light--read-increment))))
    (light
     `(,arg ,(number-to-string n)))))

(defun light-inc (&optional n)
  "docstring"
  (interactive)
  (light--apply-increment "-A" n))

(defun light-dec (&optional n)
  "docstring"
  (interactive)
  (light--apply-increment "-U" n))

;; Keybindings

(global-set-key
 (kbd "C-c n s -")
 (lambda ()
   (interactive)
   (light-dec 10)))

(global-set-key
 (kbd "C-c n s +")
 (lambda ()
   (interactive)
   (light-inc 10)))

(provide 'light)
