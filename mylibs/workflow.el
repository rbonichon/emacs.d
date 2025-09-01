(defun wf--get-task ()
  "Retrieve the task from the current org heading.

Returns the task's title and link if any
"
  (save-excursion
    (org-back-to-heading)
    (org-element-at-point))
  )

(defun wf--create-new-task-part (task &optional no-clock-in)
  "docstring"
  (save-excursion
    (when (equal 'headline (car task))
      (let* ((hdr (org-element-property
                   :title task)))
        ;; Retrieve current org-roam-dailies file for today. Create it if needed
        (org-roam-dailies-find-today)
        ;; Add a node with the task's title. Maybe link back to original task. I
        ;; don't know if it's useful
        (end-of-buffer)
        (org-insert-heading)
        (insert (format " %s" hdr))
        ;; Get a link from the current org-dailies heading
        (beginning-of-line)
        (setq link
              (org-store-link nil nil))
        ;; Clock in unless absolutely told not to
        (unless no-clock-in
          (org-clock-in))
        ;;
        ))))

(defun wf-do-task (&optional no-clock-in)
  "Do a given task following the workflow's rules. This switches the
focus to the day's org buffer."
  (interactive)
  ;; Use task at point in an org mode. Assume it it the one starting at the current line
  (let* ((task (wf--get-task))
         (new-task-link (wf--create-new-task-part task)))
    ;; Add link to the LOGBOOK drawer
    (save-excursion
      (goto-char
       (org-log-beginning t))
      (insert
       (format "EFFORT: %s" link))
      (newline))))

