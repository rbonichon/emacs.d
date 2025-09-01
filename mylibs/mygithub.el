;; gh based utilities

(defun get-resources (repo resource-type)
  "docstring"
  (let* ((default-directory repo)
         (author "@me")
         (cmd (cond ((eq resource-type 'issue)
                     (format
                      "gh issue list -A %s --json state,number,title,url,updatedAt,body --search \"sort:updated-desc\""
                      author))
                    ((eq resource-type 'pr)
                     (format
                      "gh pr list -A @me -L 50 --json isDraft,state,number,title,url,updatedAt,body --search \"sort:updated-desc\""
                      author))
                    ((eq resource-type 'merged-pr)
                     (format
                      "gh pr list -A @me -L 50 --json number,title,url,updatedAt --search \"sort:updated-desc\" --state merged"
                      author)
                     )
                    (t nil))))
    (when cmd
      (json-parse-string
       (shell-command-to-string cmd)))))

(defun mygh-getprs (repo)
  "Return a vector of up to 50 pull requests ordered from newest to oldest"
  (get-resources repo 'pr)
  )

(defun mygh-get-issues (repo)
  (get-resources repo 'issue)
  )

(defun mygh-pp-resource-item-as-org (ndays item)
  "Extract relevant information from ITEM, typically an issue or
a pull request, and print it as a list element in org-mode."
  (let* ((url (gethash "url" item))
         (date (gethash "updatedAt" item))
         (number (gethash "number" item))
         (title
          (string-replace
           "`"
           "="
           (gethash "title" item)))
         (state (gethash "state" item))
         (is-draft (gethash "isDraft" item))
         (item-days (time-to-days
                     (date-to-time date)))
         (now (time-to-days
               (date-to-time
                (format-time-string "%FT%T%z"))))
         (elapsed-days (- now item-days)))
    (unless (> elapsed-days ndays)
      (insert
       (format
        "- %s[[%s][#%s %s]]\n %s\n %d days ago\n"
        (if is-draft "DRAFT: " "")
        url
        number
        title
        state
        elapsed-days)))))

(defun array-emptyp (a)
  "docstring"
  (equal (length a) 0))

(defun rb/goto-org-heading (path)
  "Simple function to go to a given heading indicated by PATH.

PATH is a list of headings in a parent/child relation."
      (beginning-of-buffer)
    (mapcar (lambda (it)
              (progn (search-forward-regexp (concat "* " it))
                     (org-narrow-to-subtree
                      )
                     ))
            path)
    ;; We're now at the subtree we want to update
    (org-cut-subtree)
    (org-insert-subheading 1)
    (insert (car (reverse path)))
  )

(defun mygh-update-o1-work ()
  "docstring"
  (interactive)
  (save-excursion
    (find-file "~/documents/org/o1_prs.org")
    (erase-buffer)
    (insert "# Last updated: " (format-time-string "%F %T"))
    ;; Get PRs
    (org-insert-heading t nil t)
    (insert "O(1) pull requests")
    (org-set-tags ":work:o1:prs:")
    (mygh-get-ongoing-prs-as-org-headers o1-repositories)
    ;; Retrieve issues
    (org-insert-heading t nil t)
    (insert "O(1) issues")
    (org-set-tags ":work:o1:issues:")
    (mygh-get-ongoing-issues-as-org-headers o1-repositories)
    (save-buffer)
    )
  )


(defun mygh-print-resources (ndays repos)
  "Print information about REPOS"
  (cl-loop
   for repo in repos
   ;; when repo exists on-disk
   do
   (let* ((prs (mygh-getprs repo))
          (merged-prs (get-resources repo 'merged-pr))
          (issues (mygh-get-issues repo)))
     (message "gh:resources:%s %s %s %s" repo prs merged-prs issues)
     (when (or prs merged-prs issues)
       (insert "* =" repo "=\n\n")
       (when (array-emptyp merged-prs)
         (insert "** Merged PRsn\n")
         (mapcar (lambda (it) (mygh-pp-resource-item-as-org ndays it)) merged-prs)
         )
       ;; PRs
       (when (array-emptyp prs)
         (insert "** Current PRs\n\n")
         (mapcar
          (lambda (it) (mygh-pp-resource-item-as-org ndays it))
          prs))
       ;; issues
       (when (array-emptyp issues)
         (insert
          "\n** Current Issues\n\n")
         (mapcar
          (lambda (it) (mygh-pp-resource-item-as-org ndays it))
          issues)))
     ;;
     )))


(defvar o1-repositories '("~/mina/mina"
                          "~/mina/proof-systems/"
                          "~/mina/snarkyjs"
                          "~/mina/snarkyjs-bindings")
  "git repositories (local instances) of O(1) related-work" )


(setq o1-repositories '("~/mina/mina"
                        ))
(defun mygh-get-ongoing-issues-as-org-headers (repos)
  "Get all the opened PRs from list of repositories REPOS and print
them under the current org-heading."
  (let ((first t))
    (cl-loop
     for repo in repos do
     (let ((issues (mygh-get-issues repo))
           )
       (mapcar
        (lambda (it)
          (let* ((url (gethash "url" it))
                 (number (gethash "number" it))
                 (title (gethash "title" it))
                 (body (gethash "body" it)))
            (if first
                (org-insert-subheading 1)
              (org-insert-heading))
            (insert
             (format "[[%s][(#%s)]] %s"
                     url number
                     title))
            (org-todo "TODO")
            (newline-and-indent)
            (mygh--insert-body body)
            )
          (newline-and-indent)
          (setq first nil))
        issues)))))


;; (unless (string-empty-p (string-trim "foo")) (message "bar"))
(defun mygh--insert-body (str)
  (let ((body (string-trim str)))
    (unless (string-empty-p body)
      (insert "#+begin_src markdown")
      (newline)
      (insert (string-replace "\r" "" body))
      (newline)
      (insert "#+end_src")))
  )

(defun mygh-get-ongoing-prs-as-org-headers (repos)
  "Get all the opened PRs from list of repositories REPOS and print
them under the current org-heading."
  (let ((first t))
    (cl-loop
     for repo in repos do
     (let ((prs (mygh-getprs repo))
           )
       (mapcar
        (lambda (it)
          (let* ((url (gethash "url" it))
                 (number (gethash "number" it))
                 (title (gethash "title" it))
                 (body (gethash "body" it)))
            (if first
                (org-insert-subheading 1)
              (org-insert-heading))
            (insert
             (format "[[%s][(#%s)]] %s"
                     url number
                     title))
            (org-todo "STARTED")
            (newline-and-indent)
            (mygh--insert-body body)
            )
          (newline-and-indent)
          (setq first nil)
          )
        prs)))))


(defun rb/standup-info (ndays)
  "Fill in a standup buffer with latest PR "
  (interactive "n#days: ")
  (let* ((buf (get-buffer-create "standup.org")))
    (with-current-buffer buf
      (erase-buffer)
      (mygh-print-resources ndays o1-repositories)
      (insert "* Notes\n\n")            ; Section for miscellaneous notes
      )
    (switch-to-buffer buf)
    (org-mode)
    )
  )

(defun rb/update-o1-prs ()
  "docstring"
  (interactive)
  (run-with-timer (* 30 60) t #'mygh-update-o1-work)
  )


(require 'org)

(defun gh-create-issue-from-org-heading ()
  "docstring"
  (interactive)
  (let* ((title (nth 4 (org-heading-components)))
         (issue-name (completing-read
                      "Issue name: "
                      `(,title)))
         (default-directory (completing-read "Repository: " o1-repositories))
         (body (completing-read "Description: " nil))
         (cmd (format
               "gh issue create --title %S --body %S --assignee \"@me\" "
               issue-name body)))
    (message "On %s :: %s" default-directory cmd)
    (shell-command cmd)))



