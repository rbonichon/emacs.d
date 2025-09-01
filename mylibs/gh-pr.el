;;; gh-pr-list.el --- List GitHub PRs with review status -*- lexical-binding: t; -*-

(require 'tabulated-list)
(require 'json)
(require 'project)

(defvar gh-pr-list-buffer-name "*GitHub PRs*"
  "Name of the buffer for listing PRs.")

(defface gh-pr-draft-face
  '((t :foreground "#808080"))
  "Face for draft PRs."
  :group 'gh-pr-list)

(defun gh-pr-list--fetch-prs ()
  "Fetch open PRs from the current repository."
  (let* ((project-root (or (project-root (project-current))
                           default-directory))
         (default-directory project-root)
         (json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (output (shell-command-to-string 
                  "gh pr list --state open --json number,title,author,isDraft,url --limit 100"))
         (prs (json-read-from-string output)))
    ;; Sort: non-draft first
    (sort prs (lambda (a b)
                (let ((draft-a (alist-get 'isDraft a))
                      (draft-b (alist-get 'isDraft b)))
                  (and (not draft-a) draft-b))))))

(defun gh-pr-list--fetch-reviews (pr-number)
  "Fetch reviews for PR-NUMBER."
  (let* ((project-root (or (project-root (project-current))
                           default-directory))
         (default-directory project-root)
         (json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (output (shell-command-to-string 
                  (format "gh pr view %s --json reviews" pr-number)))
         (data (json-read-from-string output))
         (reviews (alist-get 'reviews data)))
    reviews))

(defun gh-pr-list--format-reviews (pr-number)
  "Format reviews for PR-NUMBER as a string."
  (let* ((reviews (gh-pr-list--fetch-reviews pr-number))
         (reviewer-states (make-hash-table :test 'equal)))
    
    ;; Group by reviewer and get latest state
    (dolist (review reviews)
      (let* ((author (alist-get 'author review))
             (login (alist-get 'login author))
             (state (alist-get 'state review)))
        (puthash login state reviewer-states)))
    
    ;; Format as string
    (if (= (hash-table-count reviewer-states) 0)
        "No reviews"
      (let ((review-strings '()))
        (maphash (lambda (reviewer state)
                   (push (format "%s: %s" reviewer state) review-strings))
                 reviewer-states)
        (string-join (nreverse review-strings) ", ")))))

(defun gh-pr-list--make-button (text url face)
  "Create a clickable button with TEXT that opens URL, using FACE."
  (propertize text
              'face face
              'action (lambda (_) (browse-url url))
              'follow-link t
              'help-echo url
              'mouse-face 'highlight))

(defun gh-pr-list--entries ()
  "Generate entries for tabulated-list."
  (let ((prs (gh-pr-list--fetch-prs))
        (entries '()))
    (dolist (pr prs)
      (let* ((number (alist-get 'number pr))
             (title (alist-get 'title pr))
             (author-alist (alist-get 'author pr))
             (author (alist-get 'login author-alist))
             (is-draft (alist-get 'isDraft pr))
             (url (alist-get 'url pr))
             (face (if is-draft 'gh-pr-draft-face 'default))
             (draft-prefix (if is-draft "[DRAFT] " ""))
             (reviews (gh-pr-list--format-reviews number)))
        
        (push (list number
                    (vector
                     (propertize (format "#%d" number) 'face face)
                     (gh-pr-list--make-button 
                      (concat draft-prefix title) 
                      url 
                      face)
                     (propertize author 'face face)
                     (propertize reviews 'face face)))
              entries)))
    (nreverse entries)))

(defun gh-pr-list-refresh ()
  "Refresh the PR list."
  (interactive)
  (let ((inhibit-read-only t))
    (message "Fetching PRs...")
    (setq tabulated-list-entries (gh-pr-list--entries))
    (tabulated-list-print t)
    (message "Done fetching PRs.")))

(define-derived-mode gh-pr-list-mode tabulated-list-mode "GitHub PRs"
  "Major mode for listing GitHub PRs with review status."
  (setq tabulated-list-format
        [("PR" 8 t)
         ("Title" 50 t)
         ("Author" 15 t)
         ("Reviews" 40 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "PR" nil))
  (tabulated-list-init-header)
  (setq tabulated-list-entries (gh-pr-list--entries)))

;;;###autoload
(defun gh-pr-list ()
  "Display a list of GitHub PRs with review status."
  (interactive)
  (let ((buffer (get-buffer-create gh-pr-list-buffer-name)))
    (with-current-buffer buffer
      (gh-pr-list-mode)
      (gh-pr-list-refresh))
    (switch-to-buffer buffer)))

(provide 'gh-pr-list)
;;; gh-pr-list.el ends here
