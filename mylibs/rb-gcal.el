;;; rb-gcal.el --- Push org calendar entries to Google Calendar -*- lexical-binding: t; -*-

;; Author: Richard
;; Keywords: calendar, org
;; Version: 0.1.0

;;; Commentary:

;; Exports org entries tagged `rb-gcal-tag' to an ics file, then hands that
;; file to gcalcli.  Entry points:
;;
;;   M-x rb-gcal-export   write the ics only
;;   M-x rb-gcal-sync     export + import into `rb-gcal-calendar'
;;                        (C-u: --dump, i.e. show what would be sent)
;;
;; Headless, for a cron job or a just recipe:
;;
;;   emacs --batch -l ~/.emacs.d/mylibs/rb-gcal.el -f rb-gcal-batch
;;
;; Idempotence.  Every event carries a UID derived from the entry's org ID
;; (created on the fly when missing), and gcalcli's import operation keys on
;; iCalUID: re-running updates events in place instead of duplicating them.
;; The org files are saved after a run because of those new ID properties.
;;
;; Recurrence.  Two kinds of repeating entries are understood:
;;
;;   - a repeater cookie, e.g. <2026-08-28 Fri 15:15-16:05 +1w>, mapped
;;     directly onto RRULE:FREQ=WEEKLY;INTERVAL=1;
;;
;;   - a diary sexp, e.g. <%%(vacances-scolaires-activite-p date 5 '(9 18 2026))
;;     17:00-18:00>, which no ics library can translate.  The sexp is evaluated
;;     over `rb-gcal-horizon' and the matching dates are grouped into runs of
;;     consecutive weeks, each emitted as its own bounded weekly series.  A
;;     school-year activity thus becomes a handful of series split by the school
;;     holidays, rather than sixty loose events.
;;
;; The run decomposition exists because gcalcli forwards RRULE but drops EXDATE
;; and RDATE (see its ics.py): exclusions expressed that way would be lost, and
;; the activity would show up during the holidays.

;;; Code:

(require 'org)
(require 'org-id)
(require 'org-element)
(require 'calendar)
(require 'cl-lib)

(defgroup rb-gcal nil
  "Export org entries to Google Calendar via gcalcli."
  :group 'org
  :prefix "rb-gcal-")

(defcustom rb-gcal-files (list (expand-file-name "~/documents/org/tasks.org"))
  "Org files scanned for entries to export."
  :type '(repeat file))

(defcustom rb-gcal-tag "gcal"
  "Only entries carrying this tag are exported (inheritance applies)."
  :type 'string)

(defcustom rb-gcal-calendar "org"
  "Name of the target Google calendar.
Keep this a dedicated calendar, never the main one: it makes a bad run
recoverable by wiping the calendar and re-importing."
  :type 'string)

(defcustom rb-gcal-timezone "Europe/Paris"
  "IANA timezone named in the TZID of timed events.
Only ever emitted as a label for Google to interpret; it is never
resolved locally, so no tz database is needed."
  :type 'string)

(defcustom rb-gcal-ics-file (expand-file-name "~/.cache/rb-gcal/org.ics")
  "Where the generated ics file is written."
  :type 'file)

(defcustom rb-gcal-program "gcalcli"
  "The gcalcli executable."
  :type 'string)

(defcustom rb-gcal-horizon '(-31 . 550)
  "Window over which diary sexps are evaluated, in days relative to today.
Dates outside a sexp's own validity simply yield nil, so a generous
horizon is self-limiting."
  :type '(cons integer integer))

(defcustom rb-gcal-default-duration 60
  "Minutes given to a timed event whose timestamp has no end time."
  :type 'integer)

(defconst rb-gcal--uid-domain "rb-gcal.org"
  "Right-hand side of generated UIDs.")


;;; ics primitives

(defun rb-gcal--escape (s)
  "Escape S for an ics TEXT value (RFC 5545 §3.3.11)."
  (setq s (replace-regexp-in-string "\\\\" "\\\\\\\\" s))
  (setq s (replace-regexp-in-string "\\([;,]\\)" "\\\\\\1" s))
  (replace-regexp-in-string "\n" "\\\\n" s))

(defun rb-gcal--fold (line)
  "Fold LINE to 75 octets per content line (RFC 5545 §3.1)."
  (let ((out nil) (n 0))
    (dolist (c (string-to-list line))
      (let* ((s (char-to-string c))
             (b (string-bytes s)))
        (when (> (+ n b) 75)
          (push "\r\n " out)
          (setq n 1))
        (push s out)
        (setq n (+ n b))))
    (apply #'concat (nreverse out))))


;;; dates
;;
;; Absolute day numbers throughout, plus minutes-since-midnight for times:
;; both are integers, so arithmetic on them needs no special care.

(defun rb-gcal--minutes (hhmm)
  "Minutes since midnight for the \"H:MM\" string HHMM."
  (let ((parts (split-string hhmm ":")))
    (+ (* 60 (string-to-number (car parts)))
       (string-to-number (cadr parts)))))

(defun rb-gcal--fmt-date (abs)
  "Format absolute day ABS as YYYYMMDD."
  (let ((g (calendar-gregorian-from-absolute abs)))
    (format "%04d%02d%02d" (nth 2 g) (nth 0 g) (nth 1 g))))

(defun rb-gcal--fmt-datetime (abs min)
  "Format absolute day ABS at MIN minutes as a local YYYYMMDDTHHMMSS."
  (let ((abs (+ abs (/ min 1440)))
        (min (% min 1440)))
    (format "%sT%02d%02d00" (rb-gcal--fmt-date abs) (/ min 60) (% min 60))))

(defun rb-gcal--today ()
  (let ((now (decode-time)))
    (calendar-absolute-from-gregorian
     (list (nth 4 now) (nth 3 now) (nth 5 now)))))


;;; diary sexps

(defun rb-gcal--sexp-dates (form from to)
  "Absolute days between FROM and TO on which FORM evaluates non-nil."
  (let ((out nil))
    ;; `date' and `entry' are the names the diary sexp convention imposes;
    ;; `cl-progv' binds them dynamically so `eval' below can resolve them.
    (cl-loop for abs from from to to do
             (when (cl-progv '(date entry)
                       (list (calendar-gregorian-from-absolute abs) "")
                     (ignore-errors (eval form)))
               (push abs out)))
    (nreverse out)))

(defun rb-gcal--weekly-runs (dates)
  "Group sorted DATES into runs of consecutive weeks, as (FIRST . LAST) pairs."
  (let ((runs nil) (start nil) (prev nil))
    (dolist (a dates)
      (cond ((null start) (setq start a prev a))
            ((= a (+ prev 7)) (setq prev a))
            (t (push (cons start prev) runs)
               (setq start a prev a))))
    (when start (push (cons start prev) runs))
    (nreverse runs)))


;;; reading org entries

(defun rb-gcal--summary ()
  "Heading at point, stripped of todo keyword, priority, tags and timestamps.
Some headings carry their timestamp inline, sometimes behind a literal
\"SCHEDULED:\" that is not a real planning line; both are removed."
  (let ((s (org-get-heading t t t t)))
    (setq s (replace-regexp-in-string org-ts-regexp-both "" s))
    (setq s (replace-regexp-in-string "\\(SCHEDULED\\|DEADLINE\\):" "" s))
    (setq s (replace-regexp-in-string "\\`[ \t]*@[[:alnum:]_-]+[ \t]+" "" s))
    (string-trim (replace-regexp-in-string "[ \t]+" " " s))))

(defun rb-gcal--description ()
  "Body of the entry at point: prose only, no drawers, planning or timestamps."
  (let* ((beg (save-excursion (org-end-of-meta-data t) (point)))
         (end (save-excursion (org-end-of-subtree t t)))
         (lines nil))
    (when (< beg end)
      (dolist (l (split-string (buffer-substring-no-properties beg end) "\n"))
        (unless (or (string-match-p "\\`[ \t]*\\'" l)
                    (string-match-p "\\`[ \t]*:\\(PROPERTIES\\|LOGBOOK\\|END\\):" l)
                    (string-match-p "\\`[ \t]*\\(CLOCK\\|SCHEDULED\\|DEADLINE\\):" l)
                    (string-match-p "\\`[ \t]*<" l)
                    (string-match-p "\\`\\*+ " l))
          (push (string-trim l) lines))))
    (string-join (nreverse lines) "\n")))

(defun rb-gcal--parse-sexp (limit)
  "Parse a diary sexp timestamp, point being just after its `<%%'."
  (let ((form (read (current-buffer)))
        (start nil) (end nil))
    (when (looking-at "[ \t]*\\([0-9]?[0-9]:[0-9][0-9]\\)\\(?:-\\([0-9]?[0-9]:[0-9][0-9]\\)\\)?")
      (setq start (rb-gcal--minutes (match-string 1)))
      (when (match-string 2)
        (setq end (rb-gcal--minutes (match-string 2)))))
    (when (< (point) limit)
      (list :kind 'sexp :form form :start start :end end))))

(defun rb-gcal--parse-plain (limit)
  "Parse the first active timestamp before LIMIT into a plist."
  (when (re-search-forward org-ts-regexp limit t)
    (goto-char (match-beginning 0))
    (let* ((ts (org-element-timestamp-parser))
           (hour (org-element-property :hour-start ts))
           (day (calendar-absolute-from-gregorian
                 (list (org-element-property :month-start ts)
                       (org-element-property :day-start ts)
                       (org-element-property :year-start ts))))
           (day-end (when (org-element-property :year-end ts)
                      (calendar-absolute-from-gregorian
                       (list (org-element-property :month-end ts)
                             (org-element-property :day-end ts)
                             (org-element-property :year-end ts)))))
           (hour-end (org-element-property :hour-end ts)))
      (list :kind 'plain
            :day day
            :day-end (or day-end day)
            :start (when hour (+ (* 60 hour) (org-element-property :minute-start ts)))
            :end (when hour-end (+ (* 60 hour-end) (org-element-property :minute-end ts)))
            :repeat-value (org-element-property :repeater-value ts)
            :repeat-unit (org-element-property :repeater-unit ts)))))

(defun rb-gcal--timestamp ()
  "Timestamp driving the entry at point: a diary sexp if any, else the first
active timestamp.  Planning lines and inline heading timestamps both qualify."
  (let ((limit (save-excursion (org-end-of-subtree t t))))
    (or (save-excursion
          (goto-char (line-beginning-position))
          (when (re-search-forward "<%%" limit t)
            (rb-gcal--parse-sexp limit)))
        (save-excursion
          (goto-char (line-beginning-position))
          (rb-gcal--parse-plain limit)))))

(defun rb-gcal--entry ()
  "Collect the entry at point, or nil when it carries no usable timestamp."
  (let ((ts (rb-gcal--timestamp)))
    (when ts
      (list :uid (org-id-get (point) t)
            :summary (rb-gcal--summary)
            :description (rb-gcal--description)
            :ts ts))))

(defun rb-gcal-entries ()
  "Every tagged entry across `rb-gcal-files'."
  (let ((org-use-tag-inheritance t))
    (delq nil (org-map-entries #'rb-gcal--entry rb-gcal-tag
                               (mapcar #'expand-file-name rb-gcal-files)))))


;;; emitting

(defun rb-gcal--vevent (uid summary description day start end &optional rrule)
  "One VEVENT.  DAY and END are absolute days, START/END minutes or nil.
A nil START means an all-day event."
  (let ((lines nil))
    (push "BEGIN:VEVENT" lines)
    (push (format "UID:%s@%s" uid rb-gcal--uid-domain) lines)
    (push (format "DTSTAMP:%s" (format-time-string "%Y%m%dT%H%M%SZ" nil t)) lines)
    (if start
        (progn
          (push (format "DTSTART;TZID=%s:%s" rb-gcal-timezone
                        (rb-gcal--fmt-datetime day start))
                lines)
          (push (format "DTEND;TZID=%s:%s" rb-gcal-timezone
                        (rb-gcal--fmt-datetime day (or end (+ start rb-gcal-default-duration))))
                lines))
      (push (format "DTSTART;VALUE=DATE:%s" (rb-gcal--fmt-date day)) lines)
      (push (format "DTEND;VALUE=DATE:%s" (rb-gcal--fmt-date (1+ (or end day)))) lines))
    (when rrule (push (concat "RRULE:" rrule) lines))
    (push (format "SUMMARY:%s" (rb-gcal--escape summary)) lines)
    (unless (string-empty-p description)
      (push (format "DESCRIPTION:%s" (rb-gcal--escape description)) lines))
    (push "END:VEVENT" lines)
    (mapcar #'rb-gcal--fold (nreverse lines))))

(defun rb-gcal--freq (unit)
  (pcase unit
    ('hour "HOURLY") ('day "DAILY") ('week "WEEKLY")
    ('month "MONTHLY") ('year "YEARLY")))

(defun rb-gcal--sexp-vevents (ev ts)
  "VEVENTs for a diary-sexp entry: one bounded weekly series per run."
  (let* ((today (rb-gcal--today))
         (dates (rb-gcal--sexp-dates (plist-get ts :form)
                                     (+ today (car rb-gcal-horizon))
                                     (+ today (cdr rb-gcal-horizon))))
         (start (plist-get ts :start))
         (end (plist-get ts :end))
         (out nil))
    (dolist (run (rb-gcal--weekly-runs dates))
      (let* ((first (car run))
             (last (cdr run))
             ;; UID keyed on the run's first day: adding future school years
             ;; appends runs without disturbing the ones already pushed.
             (uid (format "%s-%s" (plist-get ev :uid) (rb-gcal--fmt-date first)))
             ;; COUNT, not UNTIL: UNTIL must be expressed in UTC, and
             ;; converting to UTC needs the tz database to resolve
             ;; `rb-gcal-timezone' by name -- which fails silently where
             ;; TZDIR is unset.  The occurrence count is already known
             ;; exactly, so it bounds the series without any conversion.
             (rrule (unless (= first last)
                      (format "FREQ=WEEKLY;INTERVAL=1;COUNT=%d"
                              (1+ (/ (- last first) 7))))))
        (push (rb-gcal--vevent uid (plist-get ev :summary) (plist-get ev :description)
                               first start end rrule)
              out)))
    (nreverse out)))

(defun rb-gcal--plain-vevents (ev ts)
  "VEVENTs for a plain timestamp, recurring or not."
  (let* ((freq (rb-gcal--freq (plist-get ts :repeat-unit)))
         (rrule (when freq
                  (format "FREQ=%s;INTERVAL=%d" freq (plist-get ts :repeat-value)))))
    (list (rb-gcal--vevent (plist-get ev :uid)
                           (plist-get ev :summary)
                           (plist-get ev :description)
                           (plist-get ts :day)
                           (plist-get ts :start)
                           (if (plist-get ts :start)
                               (plist-get ts :end)
                             (plist-get ts :day-end))
                           rrule))))

(defun rb-gcal--vevents (ev)
  (let ((ts (plist-get ev :ts)))
    (if (eq (plist-get ts :kind) 'sexp)
        (rb-gcal--sexp-vevents ev ts)
      (rb-gcal--plain-vevents ev ts))))

(defun rb-gcal--calendar (entries)
  "The whole VCALENDAR, as a string."
  (let ((lines (list "BEGIN:VCALENDAR"
                     "VERSION:2.0"
                     "PRODID:-//rb-gcal//org export//EN"
                     "CALSCALE:GREGORIAN"
                     (format "X-WR-TIMEZONE:%s" rb-gcal-timezone))))
    (dolist (ev entries)
      (dolist (vevent (rb-gcal--vevents ev))
        (setq lines (append lines vevent))))
    (setq lines (append lines (list "END:VCALENDAR")))
    (concat (string-join lines "\r\n") "\r\n")))


;;; commands

;;;###autoload
(defun rb-gcal-export ()
  "Write the ics file and return its path."
  (interactive)
  (let* ((entries (rb-gcal-entries))
         (ics (rb-gcal--calendar entries)))
    (org-save-all-org-buffers)          ; persist any freshly created IDs
    (make-directory (file-name-directory rb-gcal-ics-file) t)
    (with-temp-file rb-gcal-ics-file
      (set-buffer-file-coding-system 'utf-8-unix)
      (insert ics))
    (message "rb-gcal: %d entr%s -> %s"
             (length entries) (if (= 1 (length entries)) "y" "ies") rb-gcal-ics-file)
    rb-gcal-ics-file))

;;;###autoload
(defun rb-gcal-sync (&optional dump)
  "Export, then import into `rb-gcal-calendar'.
With a prefix argument DUMP, ask gcalcli to show what it would send
instead of sending it."
  (interactive "P")
  (let ((file (rb-gcal-export))
        (buffer (get-buffer-create "*rb-gcal*")))
    (with-current-buffer buffer (erase-buffer))
    (let ((status (apply #'call-process rb-gcal-program nil buffer nil
                         (append (list "import" "--calendar" rb-gcal-calendar
                                       "--verbose")
                                 (when dump '("--dump"))
                                 (list file)))))
      (if (eq status 0)
          (message "rb-gcal: %s" (if dump "dumped (nothing sent)" "imported"))
        (display-buffer buffer)
        (user-error "rb-gcal: gcalcli exited with %s" status))
      status)))

;;;###autoload
(defun rb-gcal-batch ()
  "Entry point for `emacs --batch -f rb-gcal-batch'."
  (rb-gcal-sync)
  (with-current-buffer "*rb-gcal*"
    (princ (buffer-string))))

(provide 'rb-gcal)
;;; rb-gcal.el ends here
