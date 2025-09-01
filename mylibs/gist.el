;; TODO: possibly return the gist link to share later
(defun gh--create-gist (filename &optional descr)
  "docstring"
  (let* ((descr
          (or descr (completing-read "Description: " nil)))
         (cmd (format "gh gist create %s -d %S" tmpfile descr))
         )
    (message (format "Running gh command: %s" cmd))
    (async-shell-command cmd)
    )
  )



;; TODO: derive name from tangle parameters when present
(defun org-babel-gist ()
  "When on an org src block, create a gist with its contents."
  (interactive)
  (let ((org-contents (org-element-at-point)))
    (when (equal 'src-block (car org-contents))
      (let* ( (contents
               (org-element-property :value org-contents))
              (params (org+-header-args org-contents))
              (tangle (assoc :tangle params))
              (tangle-name (when tangle (cdr tangle)))
              (name
               (if tangle-name
                   (file-name-base tangle-name)
                 (or
                  (format "%s" (org-element-property :name org-contents))
                  "gist"))
               )
              (extension
               (if tangle-name
                   (file-name-extension tangle-name)
                 (org-element-property :language org-contents)))
              (tmpfile
               (make-temp-file
                (format "%s_" name)
                nil                     ; this is not a directory
                (format ".%s" extension) contents)))
        (message (format "Created gist in %s" tmpfile) )
        (gh--create-gist tmpfile)
        ))
    )
  )

(provide 'gist)
;; Define gist-region / gist-buffer
