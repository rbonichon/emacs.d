(org-super-agenda-mode)
(let ((org-super-agenda-groups
       '((:log t)                       ; Automatically named "Log"
         (:name "Schedule"
                :time-grid t)
         (:name "Today"
                :scheduled today)
         (:habit t)
         (:name "Due today"
                :deadline today)
         (:name "Overdue"
                :deadline past)
         (:name "Due soon"
                :deadline future)
         (:name "Unimportant"
                :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH")
                :order 100)
         (:name "Waiting..."
                :todo "WAITING"
                :order 98)
         (:name "Scheduled earlier"
                :scheduled past))))
  (org-agenda-list))

(let ((org-agenda-span 'day)
      (org-super-agenda-groups
       '((:name "Time grid items in all-uppercase with RosyBrown1 foreground"
                :time-grid t
                :transformer (--> it
                                  (upcase it)
                                  (propertize it 'face '(:foreground "RosyBrown1"))))
         (:name "Priority >= C items underlined, on black background"
                :face (:background "black" :underline t)
                :not (:priority>= "C")
                :order 100))))
  (org-agenda nil "a"))


(setq spacemacs-theme-org-agenda-height nil
      org-agenda-time-grid '((daily today require-timed) "----------------------" nil)
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-include-diary t
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      org-agenda-start-with-log-mode t)

(set-face-attribute 'org-super-agenda-header nil
                    :weight 'bold
                    :background "#eeeeee" :height 1.3 :slant 'italic  :box t)  

(setq org-agenda-custom-commands
      '(("z" "Super zaen view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(
                          (:name "Next to do"
                                 :todo "NEXT"
                                 :order 1)
                          (:name "Important"
                                 :tag "Important"
                                 :priority "A"
                                 :order 6)
                          (:name "Due Today"
                                 :deadline today
                                 :order 2)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 8)
                          (:name "Overdue"
                                 :deadline past
                                 :order 7)
                          (:name "Scheduled" :scheduled future :order 9)
                          (:name "Loopless"
                                 :tag "loopless"
                                 :order 30)
                          (:name "O(1) Labs"
                                 :tag "o1labs"
                                 :order 30)
                          (:name "Assignments"
                                 :tag "Assignment"
                                 :order 10)
                          (:name "Issues"
                                 :tag "issue"
                                 :order 12)
                          (:name "Pull/Merge requests"
                                 :tag ("PR" "prs" "mr")
                                 :order 12)
                          (:name "Projects"
                                 :tag "Project"
                                 :order 14)
                          (:name "Emacs"
                                 :tag "Emacs"
                                 :order 13)
                          (:name "Research"
                                 :tag "Research"
                                 :order 15)
                          (:name "To read"
                                 :tag "Read"
                                 :order 30)
                          (:name "Waiting"
                                 :todo "WAITING"
                                 :order 20)
                          (:name "trivial"
                                 :priority<= "C"
                                 :tag ("Trivial" "Unimportant")
                                 :todo ("SOMEDAY" )
                                 :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))
