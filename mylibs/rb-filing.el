;;; rb-filing.el --- Rule-based filing of documents -*- lexical-binding: t; -*-

;; Author: Richard
;; Keywords: convenience, files, dired
;; Version: 0.1.0

;;; Commentary:

;; Recipes that say "a file named like THIS belongs THERE".
;;
;; A recipe (see `rb-filing-rules') is a plist:
;;
;;   :name        human readable label, for messages and the log
;;   :regexp      matched against the base name of the file
;;   :destination directory, either a template string in which \1 .. \9
;;                are replaced by the corresponding groups of :regexp,
;;                or a function of one argument (the absolute file name)
;;                returning a directory -- keep it pure, Dired calls it
;;                several times while building a single prompt
;;   :rename      optional new base name, same template syntax
;;
;; The engine is plain Elisp, so the same recipes drive both an interactive
;; and an unattended path:
;;
;;   - `rb-filing-dired-do-file' files the marked files, no shell involved;
;;   - `rb-filing-shell-command-guess', added to `shell-command-guess-functions'
;;     by `rb-filing-setup', makes "mkdir -p DIR && mv -n -- ? DIR" the first
;;     command `!' offers on a matching file (the usual guesses stay one M-n
;;     away).  Dired does not redisplay after `!', so type g afterwards;
;;   - `rb-filing-dwim-target-directories', as the value of `dired-dwim-target',
;;     makes `R' default to the recipe directory;
;;   - `rb-filing-sweep' files whole directories, on a timer with
;;     `rb-filing-auto-mode' or on file system events with `rb-filing-watch-mode'.
;;
;; This file only defines things; see the Dired section of emacs26.org for the
;; keybinding, the Dired options `R' needs, and the sweeper being turned on.
;; Everything the sweep does is appended to `rb-filing-log-buffer-name'.

;;; Code:

(require 'cl-lib)
(require 'filenotify)

(declare-function dired-get-marked-files "dired")
(declare-function dired-dwim-target-next "dired-aux")
(defvar shell-command-guess-functions)

;;; Customization

(defgroup rb-filing nil
  "Rule-based filing of documents."
  :group 'files
  :prefix "rb-filing-")

(defcustom rb-filing-rules
  '((:name "TVA (loopless)"
     :regexp "\\`TVA_\\([0-9]\\{2\\}\\)-\\([0-9]\\{4\\}\\)_loopless\\.pdf\\'"
     :destination "~/loopless/accounting/\\2/tva"))
  "Filing recipes, tried in order; the first match wins.
See the commentary at the top of rb-filing.el for the plist keys."
  :type '(repeat plist))

(defcustom rb-filing-case-fold t
  "Whether a rule `:regexp' matches regardless of case."
  :type 'boolean)

(defcustom rb-filing-on-conflict 'ask
  "What to do when the destination file already exists, interactively."
  :type '(choice (const :tag "Ask" ask)
                 (const :tag "Skip the file" skip)
                 (const :tag "Overwrite" overwrite)
                 (const :tag "Add a numeric suffix" unique)))

(defcustom rb-filing-sweep-on-conflict 'unique
  "Like `rb-filing-on-conflict', but for the unattended sweep.
`ask' is not honoured here: a timer must never block on a prompt."
  :type '(choice (const :tag "Skip the file" skip)
                 (const :tag "Overwrite" overwrite)
                 (const :tag "Add a numeric suffix" unique)))

(defcustom rb-filing-watch-directories '("~/Downloads")
  "Directories swept by `rb-filing-sweep' when called without arguments."
  :type '(repeat directory))

(defcustom rb-filing-ignore-regexp
  "\\`\\.\\|\\.\\(part\\|crdownload\\|tmp\\)\\'"
  "Files whose base name matches this are never filed by the sweep."
  :type 'regexp)

(defcustom rb-filing-min-age 10
  "Only sweep files that have not been modified for that many seconds.
Keeps the sweeper from grabbing a download that is still in flight."
  :type 'number)

(defcustom rb-filing-sweep-interval 300
  "Seconds between two sweeps when `rb-filing-auto-mode' is on."
  :type 'number)

(defcustom rb-filing-log-buffer-name "*rb-filing*"
  "Buffer where filing operations are logged."
  :type 'string)

(defcustom rb-filing-after-file-functions nil
  "Functions called after a file has been filed.
Each is called with three arguments: the old absolute name, the new
absolute name, and the rule that matched."
  :type 'hook)

;;; Matching and planning

(defun rb-filing--groups (rule file)
  "Return the groups RULE's regexp matches in the base name of FILE.
The result is the list of \\0 .. \\9, or nil when RULE does not apply."
  (let ((regexp (plist-get rule :regexp))
        (name (file-name-nondirectory file))
        (case-fold-search rb-filing-case-fold))
    (when (and (stringp regexp) (string-match regexp name))
      (cl-loop for i from 0 to 9 collect (match-string i name)))))

(defun rb-filing--expand (template groups)
  "Replace \\1 .. \\9 in TEMPLATE with the corresponding GROUPS."
  (replace-regexp-in-string
   "\\\\\\([0-9]\\)"
   (lambda (reference)
     (or (nth (string-to-number (substring reference 1)) groups) ""))
   template t t))

(defun rb-filing-plan (file)
  "Return how FILE should be filed, or nil if no rule matches.
The plan is a plist with :file, :rule, :directory and :target."
  (cl-loop
   for rule in rb-filing-rules
   for groups = (rb-filing--groups rule file)
   when groups
   return (let* ((destination (plist-get rule :destination))
                 (directory (file-name-as-directory
                             (expand-file-name
                              (cond
                               ((functionp destination)
                                (funcall destination file))
                               ((stringp destination)
                                (rb-filing--expand destination groups))
                               (t (error "Rule %S has no usable :destination"
                                         (or (plist-get rule :name) rule)))))))
                 (rename (plist-get rule :rename)))
            (list :file (expand-file-name file)
                  :rule rule
                  :directory directory
                  :target (expand-file-name
                           (if rename
                               (rb-filing--expand rename groups)
                             (file-name-nondirectory file))
                           directory)))))

(defun rb-filing-destination (file)
  "Return the directory FILE should be filed into, or nil."
  (plist-get (rb-filing-plan file) :directory))

;;; Applying

(defun rb-filing--unique-name (target)
  "Return TARGET, or TARGET with a numeric suffix if it already exists."
  (if (not (file-exists-p target))
      target
    (let* ((directory (file-name-directory target))
           (base (file-name-base target))
           (extension (file-name-extension target t))
           (n 1)
           candidate)
      (while (file-exists-p
              (setq candidate
                    (expand-file-name (format "%s-%d%s" base n extension)
                                      directory)))
        (setq n (1+ n)))
      candidate)))

(defun rb-filing--resolve-conflict (target conflict)
  "Return the name to write to for TARGET under the CONFLICT policy, or nil."
  (cond
   ((not (file-exists-p target)) target)
   ((eq conflict 'overwrite) target)
   ((eq conflict 'unique) (rb-filing--unique-name target))
   ((eq conflict 'ask)
    (and (y-or-n-p (format "%s already exists.  Overwrite? " target)) target))))

(defun rb-filing--log (fmt &rest args)
  "Append FMT applied to ARGS to the filing log buffer."
  (with-current-buffer (get-buffer-create rb-filing-log-buffer-name)
    (save-excursion
      (goto-char (point-max))
      (insert (format-time-string "[%F %T] ")
              (apply #'format fmt args)
              "\n"))))

(defun rb-filing-apply (file &optional dry-run conflict)
  "File FILE according to `rb-filing-rules'.
With DRY-RUN non-nil, only report what would happen.  CONFLICT overrides
`rb-filing-on-conflict'.  Return a plist with :file, :status and, when
relevant, :target and :rule.  :status is one of `filed', `would-file',
`unmatched', `skipped', `in-place' or `error'."
  (let* ((file (expand-file-name file))
         (plan (rb-filing-plan file)))
    (cond
     ((null plan) (list :file file :status 'unmatched))
     ((file-equal-p file (plist-get plan :target))
      (list :file file :status 'in-place :rule (plist-get plan :rule)))
     (t
      (let* ((rule (plist-get plan :rule))
             (name (or (plist-get rule :name) (plist-get rule :regexp)))
             (target (rb-filing--resolve-conflict
                      (plist-get plan :target)
                      ;; A dry run reports a conflict, it never asks about one.
                      (if dry-run
                          (if (eq conflict 'overwrite) 'overwrite 'skip)
                        (or conflict rb-filing-on-conflict)))))
        (cond
         ((null target)
          (rb-filing--log "skip   %s (%s exists)" file (plist-get plan :target))
          (list :file file :status 'skipped :rule rule
                :target (plist-get plan :target)))
         (dry-run
          (rb-filing--log "would  %s -> %s [%s]" file target name)
          (list :file file :status 'would-file :rule rule :target target))
         (t
          (condition-case err
              (progn
                (make-directory (plist-get plan :directory) t)
                (rename-file file target t)
                (rb-filing--log "file   %s -> %s [%s]" file target name)
                (run-hook-with-args 'rb-filing-after-file-functions
                                    file target rule)
                (list :file file :status 'filed :rule rule :target target))
            (error
             (rb-filing--log "error  %s: %s" file (error-message-string err))
             (list :file file :status 'error :rule rule
                   :error (error-message-string err)))))))))))

(defun rb-filing--summarize (results &optional dry-run)
  "Return a one line summary of RESULTS, a list of `rb-filing-apply' plists.
DRY-RUN only changes the wording."
  (cl-flet ((tally (&rest statuses)
              (cl-count-if (lambda (result)
                             (memq (plist-get result :status) statuses))
                           results)))
    (format "%s %d, unmatched %d, skipped %d, errors %d"
            (if dry-run "would file" "filed")
            (tally 'filed 'would-file)
            (tally 'unmatched)
            (tally 'skipped 'in-place)
            (tally 'error))))

;;; Sweeping directories

(defun rb-filing--sweepable-p (file)
  "Return non-nil when FILE is a regular, settled, non-ignored file."
  (and (file-regular-p file)
       (not (string-match-p rb-filing-ignore-regexp
                            (file-name-nondirectory file)))
       (or (null rb-filing-min-age)
           (> (float-time (time-since (file-attribute-modification-time
                                       (file-attributes file))))
              rb-filing-min-age))))

;;;###autoload
(defun rb-filing-sweep (&optional directories dry-run conflict)
  "Apply `rb-filing-rules' to every file of DIRECTORIES.
DIRECTORIES defaults to `rb-filing-watch-directories'.  With DRY-RUN, or
interactively with a prefix argument, only report what would be done.
CONFLICT overrides `rb-filing-sweep-on-conflict'.  Return the results."
  (interactive (list nil current-prefix-arg))
  (let* ((directories (or directories rb-filing-watch-directories))
         (conflict (or conflict rb-filing-sweep-on-conflict))
         (results
          (cl-loop
           for directory in directories
           when (file-directory-p (expand-file-name directory))
           append (cl-loop
                   for file in (directory-files (expand-file-name directory) t)
                   when (rb-filing--sweepable-p file)
                   collect (rb-filing-apply file dry-run conflict)))))
    (when (called-interactively-p 'interactive)
      (message "rb-filing: %s" (rb-filing--summarize results dry-run)))
    results))

(defun rb-filing--sweep-quietly ()
  "Sweep, and only speak up when something actually moved or broke."
  (let ((moved (cl-remove-if-not
                (lambda (result)
                  (memq (plist-get result :status) '(filed error)))
                (rb-filing-sweep))))
    (when moved
      (message "rb-filing: %s" (rb-filing--summarize moved)))))

;;; Triggers: a timer, or file system events

(defvar rb-filing--timer nil
  "Timer used by `rb-filing-auto-mode'.")

;;;###autoload
(define-minor-mode rb-filing-auto-mode
  "Sweep `rb-filing-watch-directories' every `rb-filing-sweep-interval' seconds."
  :global t
  (when (timerp rb-filing--timer)
    (cancel-timer rb-filing--timer)
    (setq rb-filing--timer nil))
  (when rb-filing-auto-mode
    (setq rb-filing--timer
          (run-with-timer rb-filing-sweep-interval rb-filing-sweep-interval
                          #'rb-filing--sweep-quietly))))

(defvar rb-filing--watches nil
  "File notification descriptors used by `rb-filing-watch-mode'.")

(defvar rb-filing--watch-timer nil
  "Debouncing timer used by `rb-filing-watch-mode'.")

(defun rb-filing--watch-callback (event)
  "Schedule a sweep after the file system EVENT settles."
  (when (memq (nth 1 event) '(created renamed changed))
    (when (timerp rb-filing--watch-timer)
      (cancel-timer rb-filing--watch-timer))
    ;; One second past `rb-filing-min-age', so the file the event is about
    ;; is old enough for `rb-filing--sweepable-p' when the sweep gets there.
    (setq rb-filing--watch-timer
          (run-with-timer (1+ (or rb-filing-min-age 0)) nil
                          #'rb-filing--sweep-quietly))))

;;;###autoload
(define-minor-mode rb-filing-watch-mode
  "Sweep `rb-filing-watch-directories' whenever one of them changes.
An event driven alternative to `rb-filing-auto-mode'.  Turn it off and on
again after changing `rb-filing-watch-directories'."
  :global t
  (mapc #'file-notify-rm-watch rb-filing--watches)
  (setq rb-filing--watches nil)
  (when (timerp rb-filing--watch-timer)
    (cancel-timer rb-filing--watch-timer)
    (setq rb-filing--watch-timer nil))
  (when rb-filing-watch-mode
    (dolist (directory rb-filing-watch-directories)
      (let ((directory (expand-file-name directory)))
        (when (file-directory-p directory)
          (push (file-notify-add-watch directory '(change)
                                       #'rb-filing--watch-callback)
                rb-filing--watches))))))

;;; Dired

;;;###autoload
(defun rb-filing-dired-do-file (&optional dry-run)
  "File the marked files (or the file at point) using `rb-filing-rules'.
With DRY-RUN, or interactively with a prefix argument, only report what
would be done."
  (interactive "P" dired-mode)
  (let ((results (mapcar (lambda (file) (rb-filing-apply file dry-run))
                         (dired-get-marked-files))))
    (when (cl-some (lambda (result) (eq (plist-get result :status) 'filed))
                   results)
      (revert-buffer))
    (message "rb-filing: %s" (rb-filing--summarize results dry-run))
    results))

(defun rb-filing--shared-plans (files)
  "Return the plans of FILES when one `mv' can file them all, else nil.
That means every file matches a rule, they share a destination, and a
`:rename' recipe is only honoured for a single file."
  (let ((plans (mapcar #'rb-filing-plan files)))
    (and plans
         (cl-every #'identity plans)
         (null (cdr (delete-dups (mapcar (lambda (plan)
                                           (plist-get plan :directory))
                                         plans))))
         (or (null (cdr plans))
             (cl-notany (lambda (plan)
                          (plist-get (plist-get plan :rule) :rename))
                        plans))
         plans)))

(defun rb-filing-shell-command-guess (commands files)
  "Put the filing command at the head of COMMANDS when FILES call for one.
A `shell-command-guess-functions' entry, so that `!' in Dired offers the
move as its default.  `?' is Dired's placeholder for each file in turn,
and `mv -n' is used because Dired runs the command with no standard
input, which would turn `mv -i' into a refusal."
  (let ((plans (rb-filing--shared-plans files)))
    (if (null plans)
        commands
      (let* ((directory (plist-get (car plans) :directory))
             (target (if (plist-get (plist-get (car plans) :rule) :rename)
                         (plist-get (car plans) :target)
                       directory)))
        (cons (format "mkdir -p %s && mv -n -- ? %s"
                      (shell-quote-argument (directory-file-name directory))
                      (shell-quote-argument target))
              commands)))))

(defun rb-filing-dwim-target-directories ()
  "Return the directories `R' should offer, the recipe destination first.
A `dired-dwim-target' value.  The recipe only applies when every file the
command acts on agrees, so that marking unrelated files cannot inherit
one recipe; the stock guess stays behind it in the future history."
  (append (let ((plans (rb-filing--shared-plans
                        (ignore-errors
                          (dired-get-marked-files nil current-prefix-arg)))))
            (and plans (list (plist-get (car plans) :directory))))
          (dired-dwim-target-next)))

;;;###autoload
(defun rb-filing-setup ()
  "Teach Dired's `!' to offer the filing command as its first guess."
  (with-eval-after-load 'dired-aux
    (add-to-list 'shell-command-guess-functions
                 #'rb-filing-shell-command-guess t)))

(provide 'rb-filing)
;;; rb-filing.el ends here
