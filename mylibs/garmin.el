;;; garmin.el --- Browse Garmin activities in a tablist -*- lexical-binding: t; -*-

;; Author: Richard Bonichon
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (tablist "1.0"))
;; Keywords: sports, garmin

;;; Commentary:
;;
;; `M-x garmin-list' opens a `tabulated-list' / `tablist' view of the Garmin
;; activities found in `garmin-activities-directory' (GarminDB's FitFiles
;; layout: `<id>_ACTIVITY.fit' + sibling `activity_<id>.json').
;;
;; The list is browsed one month at a time, starting with the current month.
;; Each line shows the activity date, its session type, whether it is a
;; marker, its title, and whether it has been analyzed — i.e. the `.fit'
;; has been extracted and materialised into a per-run org file under
;; `garmin-runs-directory' (see `garmin-emacs.el').
;;
;; The Session column is auto-derived: the Garmin activity name is matched
;; against `garmin-list--type-name-rules' (so an "Allure Seuil" workout reads
;; as "Seuil", "Endurance Fondamentale" as "EF", …), falling back to Garmin's
;; `trainingEffectLabel'.  Derived values are dimmed; press `t' to pin an
;; explicit type.  The Mark column flags reference sessions to compare
;; against; press `m' to toggle.  Both overrides are stored outside GarminDB's
;; tree so they survive a re-sync.
;;
;; Keys:
;;   x      extract the activity on the current line (runs the import pipeline)
;;   e      edit the title     t   set session type    m   toggle marker
;;   M-n    next month        M-p    previous month        .   back to today
;;   g      rescan the directory
;;
;; Only the activity view exists for now; it mirrors the shape of
;; `docker-image.el' from docker.el.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'tabulated-list)
(require 'tablist)

(defgroup garmin nil
  "Browse Garmin activities."
  :group 'applications
  :prefix "garmin-")

(defcustom garmin-activities-directory
  (expand-file-name "HealthData/FitFiles/Activities" "~")
  "Directory holding GarminDB activity files.
Expects `<id>_ACTIVITY.fit' files with sibling `activity_<id>.json'
metadata (title, start time)."
  :type 'directory
  :group 'garmin)

(defcustom garmin-runs-directory
  (expand-file-name "runs/" "~/documents/org/zknotes/")
  "Directory where per-run org files are written by the import pipeline.
Used to decide whether an activity has been analyzed.  Must match
`garmin-emacs-runs-dir'."
  :type 'directory
  :group 'garmin)

(defcustom garmin-lib-directory "~/code/garmin/"
  "Directory containing `garmin-emacs.el' (the extract/import pipeline).
Loaded on demand the first time an activity is extracted."
  :type 'directory
  :group 'garmin)

(defcustom garmin-claude-binary "claude"
  "Claude Code CLI executable used by `garmin-list-analyze'."
  :type 'string
  :group 'garmin)

(defcustom garmin-analyze-model "opus"
  "Model passed to headless Claude Code for `garmin-list-analyze'.
One of the Claude Code aliases (e.g. \"opus\", \"sonnet\")."
  :type 'string
  :group 'garmin)

(defcustom garmin-analyze-directory (expand-file-name "~/documents/org/")
  "Working directory for the headless Claude run in `garmin-list-analyze'.
Chosen so `--permission-mode acceptEdits' auto-approves edits to the
per-run and weekly-journal files, which both live under this tree."
  :type 'directory
  :group 'garmin)

(defcustom garmin-title-width 70
  "Width of the Title column; longer titles are truncated with an ellipsis."
  :type 'natnum
  :group 'garmin)

(defcustom garmin-titles-file
  (expand-file-name "~/HealthData/garmin-titles.eld")
  "File persisting user title overrides, an alist keyed by activity id.
Kept outside GarminDB's managed tree so renames survive a
`garmindb_cli.py' re-download/import, which overwrites the activity
JSON files."
  :type 'file
  :group 'garmin)

(defcustom garmin-week-journal-directory
  (expand-file-name "~/documents/org/zknotes/")
  "Org-roam directory holding the weekly running-journal files.
Files are matched by name as `running_journal_<ISO-year>_w<week>.org'."
  :type 'directory
  :group 'garmin)

(defcustom garmin-session-types-file
  (expand-file-name "~/HealthData/garmin-session-types.eld")
  "File persisting session-type overrides, an alist keyed by activity id.
An override wins over the value derived from the Garmin activity name /
`trainingEffectLabel' (see `garmin-list--derive-type').  Kept outside
GarminDB's managed tree so it survives a `garmindb_cli.py' re-sync."
  :type 'file
  :group 'garmin)

(defcustom garmin-markers-file
  (expand-file-name "~/HealthData/garmin-markers.eld")
  "File persisting the set of marker activities, a list of activity ids.
Marker sessions are reference workouts to compare later runs against.
Kept outside GarminDB's managed tree so it survives a re-sync."
  :type 'file
  :group 'garmin)

(defcustom garmin-session-type-labels
  '("EF" "EF active" "Récup" "Seuil" "Tempo" "Marathon"
    "VO2max" "Interval" "Vitesse" "Côtes" "Longue" "Compét")
  "Completion candidates offered by `garmin-list-edit-type'.
Free-form input is still accepted; this list is a convenience only."
  :type '(repeat string)
  :group 'garmin)

;;; ---- data ------------------------------------------------------------------

(defvar-local garmin-list--cache nil
  "Cached activity records, a list of plists (see `garmin-list--scan').")

(defvar-local garmin-list--month nil
  "Currently displayed month as a cons cell (YEAR . MONTH).")

(defun garmin-list--current-ym ()
  "Return the current month as a (YEAR . MONTH) cons."
  (let ((now (decode-time)))
    (cons (nth 5 now) (nth 4 now))))

(defun garmin-list--read-eld (file)
  "Read and return the single Lisp form persisted in FILE, or nil."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (ignore-errors (read (current-buffer))))))

(defun garmin-list--write-eld (file form)
  "Persist FORM to FILE as a single readable Lisp expression."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (let ((print-length nil)
          (print-level nil))
      (prin1 form (current-buffer))
      (insert "\n"))))

(defun garmin-list--load-overrides ()
  "Return the id→title override alist read from `garmin-titles-file'."
  (garmin-list--read-eld garmin-titles-file))

(defun garmin-list--save-overrides (alist)
  "Persist ALIST (id→title) to `garmin-titles-file'."
  (garmin-list--write-eld garmin-titles-file alist))

(defun garmin-list--load-session-types ()
  "Return the id→session-type override alist from `garmin-session-types-file'."
  (garmin-list--read-eld garmin-session-types-file))

(defun garmin-list--load-markers ()
  "Return the list of marker activity ids from `garmin-markers-file'."
  (garmin-list--read-eld garmin-markers-file))

(defun garmin-list--slug (ts)
  "Return the 14-digit timestamp slug of TS, or nil.
\"2026-06-22 07:04:20\" → \"20260622070420\"."
  (when ts
    (let ((digits (replace-regexp-in-string "[^0-9]" "" ts)))
      (when (>= (length digits) 14)
        (substring digits 0 14)))))

(defun garmin-list--analyzed-p (record)
  "Non-nil when RECORD's activity has a per-run org file.
The org filename is derived from the GMT start time, matching
`garmin-emacs--target-filename'."
  (when-let* ((file (garmin-list--run-file record)))
    (file-exists-p file)))

(defun garmin-list--run-file (record)
  "Return the per-run org path for RECORD, whether or not it exists."
  (when-let* ((slug (garmin-list--slug (plist-get record :gmt))))
    (expand-file-name (concat slug "-run.org") garmin-runs-directory)))

(defun garmin-list--week-file (record)
  "Return the weekly running-journal file covering RECORD's date, or nil."
  (when-let* ((local (plist-get record :local))
              (dir (expand-file-name garmin-week-journal-directory))
              ((file-directory-p dir)))
    (let* ((time (date-to-time local))
           (rx (format "running_journal_%s_w0*%d\\.org\\'"
                       (format-time-string "%G" time)
                       (string-to-number (format-time-string "%V" time)))))
      (car (directory-files dir t rx)))))

(defconst garmin-list--type-name-rules
  '(("seuil\\|threshold\\|lactate" . "Seuil")
    ("tempo" . "Tempo")
    ("marathon" . "Marathon")
    ("repos actif\\|recovery\\|récup\\|recup" . "Récup")
    ("endurance fondamentale\\|\\bEF\\b\\|\\bbase\\b" . "EF")
    ("vo2\\|anaerobic\\|anaéro\\|anaero\\|sprint\\|\\bspeed\\b\\|vitesse" . "VO2max")
    ("côtes\\|\\bcotes\\|\\bhill" . "Côtes")
    ("[0-9]+ *x *[0-9]+\\|\\bI[0-9]+x\\|\\bR *[0-9]+\\|fractionn\\|\\binterval\\|négative split\\|negative split\\|\\bpiste" . "Interval"))
  "Ordered (REGEXP . LABEL) rules matched case-insensitively on the activity name.
The first matching regexp wins, so more specific intents come first.")

(defconst garmin-list--te-label-map
  '(("AEROBIC_BASE" . "EF")
    ("RECOVERY" . "Récup")
    ("TEMPO" . "Tempo")
    ("LACTATE_THRESHOLD" . "Seuil")
    ("VO2MAX" . "VO2max")
    ("ANAEROBIC_CAPACITY" . "VO2max")
    ("SPEED" . "Vitesse"))
  "Fallback map from Garmin's `trainingEffectLabel' to a canonical session type.")

(defun garmin-list--derive-type (record)
  "Return the auto-derived canonical session type for RECORD, or nil.
Matches the Garmin activity name against `garmin-list--type-name-rules',
falling back to Garmin's `trainingEffectLabel'.  Ignores any override."
  (let ((name (or (plist-get record :garmin-title) ""))
        (case-fold-search t))
    (or (cl-loop for (re . label) in garmin-list--type-name-rules
                 when (string-match-p re name) return label)
        (cdr (assoc (plist-get record :te-label) garmin-list--te-label-map)))))

(defun garmin-list--scan ()
  "Scan `garmin-activities-directory' into a list of activity records.
Each record is a plist with :id (the `.fit' path), :aid, :gmt, :local,
:date, :year, :month, :garmin-title, :title, :te-label, :type-override
and :marker."
  (let ((dir (expand-file-name garmin-activities-directory))
        (overrides (garmin-list--load-overrides))
        (types (garmin-list--load-session-types))
        (markers (garmin-list--load-markers))
        records)
    (dolist (json (directory-files dir t "\\`activity_[0-9]+\\.json\\'")
                  (nreverse records))
      (when-let*
          ((id (and (string-match "activity_\\([0-9]+\\)\\.json\\'" json)
                    (match-string 1 json)))
           (fit (expand-file-name (format "%s_ACTIVITY.fit" id) dir))
           ((file-exists-p fit)))
        (ignore-errors
          (let* ((data (let ((json-object-type 'alist)
                             (json-array-type 'list)
                             (json-key-type 'symbol))
                         (json-read-file json)))
                 (local (alist-get 'startTimeLocal data)))
            (when (and (stringp local) (>= (length local) 10))
              (push (list :id fit
                          :aid id
                          :gmt (alist-get 'startTimeGMT data)
                          :local local
                          :date (substring local 0 10)
                          :day (substring local 8 10)
                          :year (string-to-number (substring local 0 4))
                          :month (string-to-number (substring local 5 7))
                          :garmin-title (alist-get 'activityName data)
                          :te-label (alist-get 'trainingEffectLabel data)
                          :type-override (cdr (assoc id types))
                          :marker (and (member id markers) t)
                          :title (or (cdr (assoc id overrides))
                                     (alist-get 'activityName data)))
                    records))))))))

;;; ---- tablist view ----------------------------------------------------------

(defun garmin-list--entries ()
  "Return `tabulated-list-entries' for the currently displayed month."
  (let ((y (car garmin-list--month))
        (m (cdr garmin-list--month)))
    (cl-loop
     for r in garmin-list--cache
     when (and (= (plist-get r :year) y) (= (plist-get r :month) m))
     collect
     (list (plist-get r :id)
           (vector (plist-get r :day)
                   (garmin-list--type-cell r)
                   (if (plist-get r :marker)
                       (propertize "★" 'face 'warning)
                     "")
                   (truncate-string-to-width
                    (or (plist-get r :title) "—") garmin-title-width nil nil "...")
                   (if (garmin-list--analyzed-p r)
                       (propertize "✓ yes" 'face 'success)
                     (propertize "—" 'face 'shadow)))))))

(defun garmin-list--session-type (record)
  "Return RECORD's effective session type: user override or derived value."
  (or (plist-get record :type-override)
      (garmin-list--derive-type record)))

(defun garmin-list--type-cell (record)
  "Render RECORD's session type for the tablist.
An explicit override shows plainly; an auto-derived value is dimmed to
signal it is a guess; an unknown type shows a shadowed em-dash."
  (let ((override (plist-get record :type-override)))
    (cond
     (override override)
     ((garmin-list--derive-type record)
      (propertize (garmin-list--derive-type record) 'face 'shadow))
     (t (propertize "—" 'face 'shadow)))))

(defun garmin-list--update-header ()
  "Refresh the tab line to show the current month and key hints.
The header line is left to `tabulated-list-mode' for the column titles."
  (setq-local tab-line-format
        (format " Garmin — %04d-%02d   [M-p/M-n month] [. today] [RET run] \
[TAB week] [a analyze] [e title] [t type] [m mark] [x/X extract] [g rescan] [q quit]"
                (car garmin-list--month) (cdr garmin-list--month))))

(defun garmin-list--goto-month (year month)
  "Display activities for YEAR and MONTH and reprint."
  (setq garmin-list--month (cons year month))
  (garmin-list--update-header)
  (tabulated-list-print t))

(defun garmin-list--shift-month (delta)
  "Move the displayed month by DELTA months."
  (let ((i (+ (* (car garmin-list--month) 12)
              (1- (cdr garmin-list--month))
              delta)))
    (garmin-list--goto-month (/ i 12) (1+ (mod i 12)))))

(defun garmin-list-next-month ()
  "Show the next month."
  (interactive)
  (garmin-list--shift-month 1))

(defun garmin-list-previous-month ()
  "Show the previous month."
  (interactive)
  (garmin-list--shift-month -1))

(defun garmin-list-current-month ()
  "Jump back to the current month."
  (interactive)
  (let ((ym (garmin-list--current-ym)))
    (garmin-list--goto-month (car ym) (cdr ym))))

(defun garmin-list--revert (&optional _arg _noconfirm)
  "Rescan the directory and reprint (bound to \\[revert-buffer])."
  (setq garmin-list--cache (garmin-list--scan))
  (garmin-list--update-header)
  (tabulated-list-print t))

(defun garmin--ensure-lib ()
  "Load `garmin-emacs' from `garmin-lib-directory' if not already available."
  (unless (fboundp 'garmin-import-fit)
    (add-to-list 'load-path (expand-file-name garmin-lib-directory))
    (require 'garmin-emacs)))

(declare-function garmin-import-fit "garmin-emacs" (fit-file))
(declare-function garmin-emacs-set-run-properties "garmin-emacs" (file props))

(defun garmin-list--sync-run-properties (record)
  "Stamp RECORD's session type and marker into its per-run org file.
No-op when the activity has not been extracted yet — the values are then
written the next time it is.  The session type mirrors the tablist
column (override or derived); the marker is written as `yes' or removed."
  (when-let* ((file (garmin-list--run-file record))
              ((file-exists-p file)))
    (garmin--ensure-lib)
    (garmin-emacs-set-run-properties
     file
     (list (cons "GARMIN_SESSION_TYPE" (garmin-list--session-type record))
           (cons "GARMIN_MARKER" (and (plist-get record :marker) "yes"))))))

(defun garmin-list--record-at-point ()
  "Return the cached activity record for the line at point, or nil."
  (when-let* ((id (tabulated-list-get-id)))
    (cl-find id garmin-list--cache
             :key (lambda (r) (plist-get r :id)) :test #'equal)))

(defun garmin-list-visit-run ()
  "Open the per-run org for the activity at point, extracting it first if needed.
`garmin-import-fit' opens an existing entry or creates it on the fly, so
pressing RET on a not-yet-analyzed activity extracts it, then visits."
  (interactive)
  (let ((rec (or (garmin-list--record-at-point)
                 (user-error "No activity on this line")))
        (buf (current-buffer)))
    (garmin--ensure-lib)
    (garmin-import-fit (plist-get rec :id))
    (garmin-list--sync-run-properties rec)
    (when (buffer-live-p buf)
      (with-current-buffer buf (tabulated-list-print t)))))

(defun garmin-list-visit-week ()
  "Open the weekly running journal covering the activity at point."
  (interactive)
  (let ((rec (or (garmin-list--record-at-point)
                 (user-error "No activity on this line"))))
    (let ((file (garmin-list--week-file rec)))
      (if file
          (find-file file)
        (user-error "No week journal for %s" (plist-get rec :date))))))

(defun garmin-list-edit-title ()
  "Rename the activity at point.
The new title is stored in `garmin-titles-file' (keyed by activity id),
not in the GarminDB JSON, so it survives `garmindb_cli.py' re-syncs.
An empty title clears the override and restores the Garmin name.
Note: activities already extracted keep their original title in the
per-run org."
  (interactive)
  (let* ((rec (or (garmin-list--record-at-point)
                  (user-error "No activity on this line")))
         (aid (plist-get rec :aid))
         (garmin (plist-get rec :garmin-title))
         (old (or (plist-get rec :title) ""))
         (new (string-trim (read-string "Title: " old))))
    (unless (string= new old)
      (let ((ov (assoc-delete-all aid (copy-alist (garmin-list--load-overrides)))))
        (unless (or (string-empty-p new) (equal new garmin))
          (push (cons aid new) ov))
        (garmin-list--save-overrides ov))
      (setf (plist-get rec :title) (if (string-empty-p new) (or garmin "") new))
      (tabulated-list-print t)
      (message "garmin: title updated"))))

(defun garmin-list-edit-type ()
  "Set the session type for the activity at point (overrides the derived value).
Completes over `garmin-session-type-labels' but accepts free text.  An
empty entry — or one equal to the auto-derived type — clears the
override, restoring the derived value.  The override is stored in
`garmin-session-types-file' (keyed by activity id) so it survives
`garmindb_cli.py' re-syncs."
  (interactive)
  (let* ((rec (or (garmin-list--record-at-point)
                  (user-error "No activity on this line")))
         (aid (plist-get rec :aid))
         (derived (garmin-list--derive-type rec))
         (current (or (plist-get rec :type-override) derived ""))
         (new (string-trim
               (completing-read (format "Session type (auto: %s): " (or derived "—"))
                                garmin-session-type-labels nil nil current))))
    (let ((keep (and (not (string-empty-p new)) (not (equal new derived)) new))
          (types (assoc-delete-all aid (copy-alist (garmin-list--load-session-types)))))
      (when keep (push (cons aid keep) types))
      (garmin-list--write-eld garmin-session-types-file types)
      (setf (plist-get rec :type-override) keep)
      (garmin-list--sync-run-properties rec)
      (tabulated-list-print t)
      (message "garmin: session type %s"
               (cond (keep (format "set to %s" keep))
                     (t "cleared (auto)"))))))

(defun garmin-list-toggle-marker ()
  "Toggle whether the activity at point is a marker session.
Marker sessions are reference workouts to compare later runs against.
The set of marked ids is persisted in `garmin-markers-file', keyed by
activity id, so it survives `garmindb_cli.py' re-syncs."
  (interactive)
  (let* ((rec (or (garmin-list--record-at-point)
                  (user-error "No activity on this line")))
         (aid (plist-get rec :aid))
         (markers (delete aid (copy-sequence (garmin-list--load-markers))))
         (on (not (plist-get rec :marker))))
    (garmin-list--write-eld garmin-markers-file
                            (if on (cons aid markers) markers))
    (setf (plist-get rec :marker) on)
    (garmin-list--sync-run-properties rec)
    (tabulated-list-print t)
    (message "garmin: %s marker" (if on "set" "cleared"))))

(defun garmin-list-extract ()
  "Extract the activity on the current line via the import pipeline.
Runs `garmin-import-fit' on the line's `.fit' file, then refreshes the
analyzed column."
  (interactive)
  (let* ((rec (or (garmin-list--record-at-point)
                  (user-error "No activity on this line")))
         (fit (plist-get rec :id))
         (buf (current-buffer)))
    (garmin--ensure-lib)
    (save-window-excursion (garmin-import-fit fit))
    (garmin-list--sync-run-properties rec)
    (when (buffer-live-p buf)
      (with-current-buffer buf (tabulated-list-print t)))
    (message "garmin: extracted %s" (file-name-nondirectory fit))))

(defun garmin-list-extract-month ()
  "Extract every not-yet-analyzed activity in the displayed month."
  (interactive)
  (let* ((y (car garmin-list--month))
         (m (cdr garmin-list--month))
         (todo (cl-remove-if-not
                (lambda (r)
                  (and (= (plist-get r :year) y)
                       (= (plist-get r :month) m)
                       (not (garmin-list--analyzed-p r))))
                garmin-list--cache))
         (n (length todo)))
    (cond
     ((zerop n)
      (message "garmin: nothing to extract for %04d-%02d" y m))
     ((yes-or-no-p (format "Extract %d activit%s of %04d-%02d? "
                           n (if (= n 1) "y" "ies") y m))
      (garmin--ensure-lib)
      (save-window-excursion
        (dolist (r todo)
          (garmin-import-fit (plist-get r :id))
          (garmin-list--sync-run-properties r)))
      (tabulated-list-print t)
      (message "garmin: extracted %d activit%s" n (if (= n 1) "y" "ies"))))))

(defun garmin-list--analyze-prompt (session week)
  "Build the headless-Claude prompt analysing SESSION, summarising into WEEK."
  (concat
   "/coach_run\n\n"
   (format "Use the coach_run skill to analyze the running session recorded in\
 the org file:\n  %s\n\n" session)
   (format "Write a DETAILED analysis into that same file (%s).\n" session)
   (if week
       (format "Also write a SUMMARIZED version into the weekly running journal:\
\n  %s\n" week)
     "There is no weekly running-journal file for this week yet; write only the\
 detailed analysis, and note the missing weekly journal.\n")
   "Edit only those files."))

(defun garmin-list--revert-file (file)
  "Revert FILE's visiting buffer, if any and unmodified, to show on-disk edits."
  (when-let* ((buf (find-buffer-visiting file)))
    (with-current-buffer buf
      (unless (buffer-modified-p)
        (revert-buffer :ignore-auto :noconfirm)))))

(defun garmin-list--analyze-sentinel (session week)
  "Return a process sentinel that reverts SESSION and WEEK on success."
  (lambda (proc event)
    (when (memq (process-status proc) '(exit signal))
      (if (and (eq (process-status proc) 'exit)
               (zerop (process-exit-status proc)))
          (progn
            (garmin-list--revert-file session)
            (when week (garmin-list--revert-file week))
            (message "garmin: analysis complete — %s"
                     (file-name-nondirectory session)))
        (message "garmin: analysis FAILED (%s) — see *garmin-analyze*"
                 (string-trim event))
        (display-buffer "*garmin-analyze*")))))

(defun garmin-list-analyze ()
  "Analyze the activity at point with the coach_run skill via headless Claude.
Writes a DETAILED analysis into the per-run org and a SUMMARY into the week's
running journal.  Runs asynchronously; progress lands in `*garmin-analyze*'.
The activity must be extracted first (press `x')."
  (interactive)
  (let* ((rec (or (garmin-list--record-at-point)
                  (user-error "No activity on this line")))
         (session (or (garmin-list--run-file rec)
                      (user-error "No run-file path for this activity")))
         (week (garmin-list--week-file rec)))
    (unless (file-exists-p session)
      (user-error "Run not extracted yet — press `x' first"))
    (let* ((default-directory (file-name-as-directory
                               (expand-file-name garmin-analyze-directory)))
           (buf (get-buffer-create "*garmin-analyze*"))
           (prompt (garmin-list--analyze-prompt session week))
           (args (list "-p" prompt
                       "--permission-mode" "acceptEdits"
                       "--model" garmin-analyze-model
                       "--output-format" "text")))
      (with-current-buffer buf
        (erase-buffer)
        (insert (format "session: %s\nweek:    %s\nmodel:   %s\n\n"
                        session (or week "— (none)") garmin-analyze-model)))
      (make-process
       :name "garmin-analyze"
       :buffer buf
       :noquery t
       :command (cons garmin-claude-binary args)
       :sentinel (garmin-list--analyze-sentinel session week))
      (message "garmin: analyzing %s (async — see *garmin-analyze*)"
               (file-name-nondirectory session)))))

(defvar-keymap garmin-list-mode-map
  :doc "Keymap for `garmin-list-mode'."
  "a"   #'garmin-list-analyze
  "e"   #'garmin-list-edit-title
  "X"   #'garmin-list-extract-month
  "M-n" #'garmin-list-next-month
  "M-p" #'garmin-list-previous-month
  "."   #'garmin-list-current-month)

(defvar-keymap garmin-list--override-map
  :doc "Bindings shadowing `tablist-minor-mode-map' in Garmin buffers.
`tablist' claims RET, TAB, `t' (toggle-marks) and `m' (mark), so they
are overridden here."
  "RET" #'garmin-list-visit-run
  "TAB" #'garmin-list-visit-week
  "x"   #'garmin-list-extract
  "t"   #'garmin-list-edit-type
  "m"   #'garmin-list-toggle-marker)

(define-derived-mode garmin-list-mode tabulated-list-mode "Garmin"
  "Major mode for browsing Garmin activities, one month at a time."
  (setq tabulated-list-format
        (vector '("Day" 4 t)
                '("Session" 12 t)
                '("Mark" 4 t)
                (list "Title" garmin-title-width t)
                '("Analyzed" 10 t))
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Day" t)
        tabulated-list-entries #'garmin-list--entries)
  (setq-local revert-buffer-function #'garmin-list--revert)
  (tabulated-list-init-header)
  (tablist-minor-mode)
  ;; `tablist' binds RET/TAB in its minor-mode map, which would otherwise
  ;; shadow ours; override just those keys for this buffer.
  (setq-local minor-mode-overriding-map-alist
              `((tablist-minor-mode . ,garmin-list--override-map))))

;;;###autoload
(defun garmin-list ()
  "Browse Garmin activities from `garmin-activities-directory'."
  (interactive)
  (let ((buf (get-buffer-create "*garmin activities*")))
    (with-current-buffer buf
      (garmin-list-mode)
      (setq garmin-list--cache (garmin-list--scan))
      (unless garmin-list--month
        (setq garmin-list--month (garmin-list--current-ym)))
      (garmin-list--update-header)
      (tabulated-list-print t))
    (pop-to-buffer buf)))

;;;###autoload
(defalias 'garmin #'garmin-list)

(provide 'garmin)
;;; garmin.el ends here
