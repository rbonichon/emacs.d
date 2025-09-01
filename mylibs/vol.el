;; Thin wrapper over the vol command 

(defun vol-timestamp ()
  (format-time-string
   "%Y-%m-%dT%H:%M:%S"))

(defun vol (pcent-increment)
  "Call vol command with argument ARGS. Arguments must all be
strings."
  "Run CMD in buffer."
  (let* ((buffer-name "*vol/pactl*")
         (buf
          (get-buffer-create buffer-name))
         ;; derive name from command using the first word
         (procname "pactl")
         (cmd "pactl")
         (args
          `("set-sink-volume"
            "@DEFAULT_SINK@"
            ,pcent-increment)))
    (with-current-buffer
        buf
      (erase-buffer)
      (insert
       (format
        "* Command \"%s\" started at %s *"
        (string-join
         (cons cmd args)
         " ")
        (vol-timestamp))))
    (eval
     (append
      `(start-process
        ,procname
        ,buffer-name
        ,cmd)
      args))))

(defvar vol-increments (list 5 10 20 50)
  "Increments for vol command")

(defun vol--read-increment ()
  (string-to-number
   (completing-read
    "Vol increment: "
    vol-increments)))

(defun vol--apply-increment (sign &optional pcent)
  (let* ((pcent (or pcent (vol--read-increment)))
         (increment (format "%s%d%%" sign pcent)))
    (vol increment)))

(defun vol-inc (&optional pcent)
  "docstring"
  (interactive)
  (vol--apply-increment "+" pcent))

(defun vol-dec (&optional pcent)
  "docstring"
  (interactive)
  (vol--apply-increment "-" pcent))

;; Keybindings

(global-set-key
 (kbd "C-c n v -")
 (lambda ()
   "Decrease sound volume by 10%"
   (interactive)
   (vol-dec 10)))

(global-set-key
 (kbd "C-c n v +")
 (lambda ()
   "Increase sound volume by 10%"
   (interactive)
   (vol-inc 10)))

(provide 'vol)
