(require 'hydra)

(defun rb/org-refile-subtree (section-name)
  "Move current subtree to SECTION-NAME, omitting stars"
  ;; Move current subtree to another section
  (save-excursion
    (org-copy-subtree)
    (goto-char (point-min))
    (re-search-forward (format "\*+ %s" section-name))
    (org-paste-subtree)
    )
  ;; When succesful, remove it from current location
  (org-cut-subtree)
  )

(defun rb/org-reading-start ()
  "Mark a reading entry a being read"
  (interactive)
  (org-set-property "START_DATE" (org-read-date))
  (org-todo "READING")
  (rb/org-refile-subtre))

(defun rb/org-reading-end ()
  "Mark a reading entry as done"
  (interactive)
  (org-set-property "END_DATE" (org-read-date))
  (org-todo "DONE")
  (rb/org-refile-subtree "Done reading"))

(defhydra hydra-rb-read ()
  "Hydra to deal with tracking reads"
  ("e" rb/org-reading-end "end reading")
  ("s" rb/org-reading-start "start reading")
  )


(defun read-test (args)
  "docstring"
  (interactive )
  )
(provide 'rb-read)
