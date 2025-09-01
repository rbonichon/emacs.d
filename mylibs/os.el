;;; Describe the state of the OS

(defvar os-buffer "*OS" "Name of buffer to display the contents")

(defun os-autorandr ()
  "docstring"
  (shell-command-to-string "autorandr --current")
  )




(defun os-state (args)
  "docstring"
  
  )
