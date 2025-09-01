;;; embark-forge.el --- Embark actions for magit-forge -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Your Name
;; Keywords: convenience, vc
;; Package-Requires: ((emacs "27.1") (embark "0.20") (forge "0.3"))

;;; Commentary:

;; This package provides Embark actions for magit-forge topics (issues and PRs).
;; It allows you to act on forge topics using Embark's contextual menu system.

;;; Code:

(require 'embark)
(require 'forge)
(require 'forge-topic)
(require 'pr-review)

;;; Embark target finder for forge topics

(defun embark-forge--topic-at-point ()
  "Return the forge topic at point as an embark target."
  (when-let ((topic (forge-topic-at-point)))
    (cons 'forge-topic topic)))

(add-to-list 'embark-target-finders 'embark-forge--topic-at-point)

;;; Actions for forge topics

(defun embark-forge-browse-topic (topic)
  "Browse TOPIC in the web browser."
  (forge-browse topic))

(defun embark-forge-copy-url (topic)
  "Copy the URL of TOPIC to the kill ring."
  (let ((url (forge-get-url topic)))
    (kill-new url)
    (message "Copied: %s" url)))

(defun embark-forge-copy-number (topic)
  "Copy the number of TOPIC to the kill ring."
  (let ((number (number-to-string (oref topic number))))
    (kill-new number)
    (message "Copied topic number: %s" number)))

(defun embark-forge-copy-reference (topic)
  "Copy the reference (e.g., #123) of TOPIC to the kill ring."
  (let ((ref (format "#%s" (oref topic number))))
    (kill-new ref)
    (message "Copied reference: %s" ref)))

(defun embark-forge-visit-topic (topic)
  "Visit TOPIC in a dedicated buffer."
  (forge-visit topic))

(defun embark-forge-toggle-mark (topic)
  "Toggle mark on TOPIC."
  (forge-topic-mark topic))

(defun embark-forge-edit-title (topic)
  "Edit the title of TOPIC."
  (forge-edit-topic-title topic))

(defun embark-forge-edit-state (topic)
  "Edit the state (open/closed) of TOPIC."
  (forge-edit-topic-state topic))

(defun embark-forge-edit-labels (topic)
  "Edit the labels of TOPIC."
  (forge-edit-topic-labels topic))

(defun embark-forge-edit-assignees (topic)
  "Edit the assignees of TOPIC."
  (forge-edit-topic-assignees topic))

(defun embark-forge-create-comment (topic)
  "Create a comment on TOPIC."
  (forge-create-post topic))

(defun embark-forge-pull (topic)
  "Pull TOPIC (if it's a pull request)."
  (when (forge-pullreq-p topic)
    (forge-pull-pullreq topic)))

(defun embark-forge-checkout (topic)
  "Checkout TOPIC branch (if it's a pull request)."
  (when (forge-pullreq-p topic)
    (forge-checkout-pullreq topic)))

(defun embark-forge-branch-pullreq (topic)
  "Create a branch for TOPIC (if it's a pull request)."
  (when (forge-pullreq-p topic)
    (forge-branch-pullreq topic)))

(defun embark-forge-worktree-pullreq (topic)
  "Create a worktree for TOPIC (if it's a pull request)."
  (when (forge-pullreq-p topic)
    (forge-worktree-pullreq topic)))

(defun embark-forge-pr-review (topic)
  "Open TOPIC in pr-review (if it's a pull request)."
  (when (forge-pullreq-p topic)
    (if (fboundp 'pr-review-forge)
        (pr-review-forge topic)
      (message "pr-review is not available. Install pr-review package."))))

(defun embark-forge-delete-comment (comment)
  "Delete COMMENT."
  (when (forge-post-p comment)
    (forge-delete-comment comment)))

;; Helper function to review a topic using pr-review
(defun pr-review-forge (topic)
  "Review topic, assuming it is a pull-request, using the pr-review
package."
  (let ((url (forge-get-url topic)))
    (pr-review url)
    )
  )

;;; Embark keymap for forge topics

(defvar-keymap embark-forge-topic-map
  :doc "Keymap for actions on forge topics."
  :parent embark-general-map
  "RET" #'embark-forge-visit-topic
  "b"   #'embark-forge-browse-topic
  "c"   #'embark-forge-create-comment
  "e s" #'embark-forge-edit-state
  "e t" #'embark-forge-edit-title
  "e l" #'embark-forge-edit-labels
  "e a" #'embark-forge-edit-assignees
  "m"   #'embark-forge-toggle-mark
  "w"   #'embark-forge-copy-url
  "W"   #'embark-forge-copy-reference
  "#"   #'embark-forge-copy-number
  )

(defvar-keymap embark-forge-pullreq-map
  :doc "Keymap for actions on forge pull requests."
  :parent embark-forge-topic-map
  "p"   #'embark-forge-pull
  "C"   #'embark-forge-checkout
  "B"   #'embark-forge-branch-pullreq
  "W"   #'embark-forge-worktree-pullreq
  "r"   #'embark-forge-pr-review)

;;; Register keymaps with embark

(add-to-list 'embark-keymap-alist '(forge-topic . embark-forge-pullreq-map))
(add-to-list 'embark-keymap-alist '(forge-pullreq . embark-forge-pullreq-map))

;;; Auto-initialization

(defvar embark-forge-actions
  '(("Browse in browser" . embark-forge-browse-topic)
    ("Visit topic" . embark-forge-visit-topic)
    ("Copy URL" . embark-forge-copy-url)
    ("Copy reference" . embark-forge-copy-reference)
    ("Copy number" . embark-forge-copy-number)
    ("Create comment" . embark-forge-create-comment)
    ("Edit state" . embark-forge-edit-state)
    ("Edit title" . embark-forge-edit-title)
    ("Edit labels" . embark-forge-edit-labels)
    ("Edit assignees" . embark-forge-edit-assignees)
    ("Toggle mark" . embark-forge-toggle-mark))
  "List of available embark-forge actions.")

(provide 'embark-forge)
;;; embark-forge.el ends here
