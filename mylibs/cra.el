;; cra.el -- generation of Compte Rendu d'Activites

;; Author: Richard Bonichon <richard.bonichon@gmail.com>
;; Version: 0.1
;; Package-Requires: ((dash "1.0"))
;; Keywords: pdf, cra

;;; Commentary:
;; cra stands for Compte-Rendu d'Activites.
;; 
;; This is a small package to handle the generation of such cras, for myself,
;; from a single elisp function.

(require 'dash)
(require 'ert)

(cl-defstruct interval
  (lo nil :read-only t)
  (hi nil :read-only t))

(defun rb/gen-list (ival)
  "Expand an interval IVAL, represented by a list of exactly 2 elements,
  into its set of elements, represented as a list."
  (cl-assert (equal (length ival) 2) t "list should have exactly 2 elements")
  (let ((lo (car ival))
        (hi (cadr ival)))
    (progn
      (cl-assert (>= hi lo) t "hi should be >= lo")
      (cl-loop for i from lo upto hi collect i)))
  )

(ert-deftest
    test-gen-list
    ()
  "Test list generation"
  (let ((debug-on-error nil))           ; make assertions raise errors
    (should (equal (rb/gen-list '(1 3)) '(1 2 3)))
    (should-error (rb/gen-list '(4 2))) ; lo > hi should error
    (should-error (rb/gen-list '(4)))   ; a single list is not an interval
    ; a list of 3 elements is also not a correct interval specification
    (should-error (rb/gen-list '(2 3 4)))))


(defun rb/interval-list-to-list (l)
  "Convert a list L of elements and lists which represents
intervals to a single list of elements. This amounts to expanding
the intervals into sets of elements.

This is O(n^2) complexity. Could be more efficient but the final
lists do not contain more than 30 elements, so that it's not
terribly high priority.
  "
  (let ((acc))
    (cl-loop
     for e in l
     do (cond ((listp e)
               (setq acc
                     (append acc (rb/gen-list e))))
              (t
               
               (setq acc
                     (append acc (cons e nil)))))
     finally return acc
     ))
  )

(ert-deftest test-interval-list ()
  "Testing interval list expansion"
  (should
   (equal (rb/interval-list-to-list
           '((1 3) 5 (9 9)))
          '(1 2 3 5 9))))

(defun rb/cra-working-days (pto sick off &optional month year)
    " Type days in month as 
    PTO is paid time-out (count as work for activity report).
    SICK are sick days.
    OFF contains off days, e.g., Fridays off or other non-working days.
    "
    (let* ((working-days)
           (current-date (calendar-current-date))
           (month (or month (calendar-extract-month current-date)))
           (year (or year (calendar-extract-year current-date))))
      (message "Off days %s" off)
      (cl-loop for nday from 1 to
               (calendar-last-day-of-month month year)
               do
               (let* ((day (list month nday year))
                      (day-name (calendar-day-name day))
                      (day-num (calendar-day-of-week day))
                      (d (string-pad day-name 9))
                      (annotation 
                       (cond
                        ((or (eq day-num 0) (eq day-num 6)) "off")
                        ((member nday pto) "pto")
                        ((member nday sick) "sick")
                        ((member nday off) "off")
                        (t "work"))))
                 (push `(,d ,nday ,annotation) working-days)
                 )
               )
      (reverse working-days))
    )

  ;; (rb/cra-working-days nil nil '(12))
(setq cra-export-directory "~/Documents/prium/activity_report")

(defun rb/cra-get-buffer (month year)
  "Generate an activity report bufffer name parameterized by MONTH and YEAR."
  (get-buffer-create
   (format
     "activity_report_%s_%d.org"
     (downcase (calendar-month-name month))
     year)
   ))

(rb/cra-get-buffer 12 2023)

;;;###autoload
(defun rb/cra-generate-report (data)
  "Generate a PDF report for a given month of a year.

DATA is a property list with the following supported keys:
- :pto an interval list of paid time-off days
- :sick an interval list of sick days
- :off an interval list of off day (non PTO, non sick)
- :month an integer between 1 and 12 (defaults to current month)
- :year  a year (defaults to current year)

Example use:

'(rb/cra-generate-report '(:off (9 (26 30)) :pto ((19 23)) :month 12 :year 2022))'
"
  (let* ((pto   (rb/interval-list-to-list (plist-get data :pto)))
         (sick  (rb/interval-list-to-list (plist-get data :sick)))
         (off   (rb/interval-list-to-list (plist-get data :off)))
         (month (or (plist-get data :month)
                    (calendar-extract-month (calendar-current-date))))
         (year  (or (plist-get data :year)
                    (calendar-extract-year (calendar-current-date))))
         (daily-tbl (rb/cra-working-days pto sick off month year))
         (buffer (rb/cra-get-buffer month year)))
    (message "Writing buffer %s" (buffer-name buffer))
    (with-current-buffer buffer
      (erase-buffer)
      (insert
       (format "#+title: Activity report for %s %d
    #+options: toc:nil  date:nil
    #+latex_header: \\usepackage{booktabs}
    #+latex_header: \\usepackage{nopageno}

    #+ATTR_LATEX: :align @{}p{0.25\\textwidth}rl@{} :center t :booktabs t
    "
               (calendar-month-name month) year))
      ;; write table 
      (let* ((day-column-width (-max (--map (length (car it)) daily-tbl)))
             (status-column-width (-max (--map (length (caddr it)) daily-tbl)))
             (fmt (format "| %%-%ds | %%2d | %%-%ds |\n"
                          day-column-width status-column-width))
             (full-line 
              (concat "|" 
                      (make-string (+ day-column-width 2) ?-) 
                      "+----+"
                      (make-string (+ status-column-width 2) ?-)
                      "|\n")))
        (cl-loop 
         for day in daily-tbl
         do 
         (insert (format fmt (car day) (cadr day) (caddr day)))
         initially (insert full-line)
         finally   (insert full-line)
         ))
      (org-mode)
      (message "Emitting PDF file for buffer %s" (buffer-name buffer))
      (let ((filename
             (expand-file-name (buffer-name buffer) cra-export-directory)))
        (write-file filename)     ; write org file
        (org-latex-export-to-pdf) ; export to PDF through latex (generate tex file)
        (delete-file filename)    ; delete org file on disk
        (delete-file (concat (file-name-sans-extension filename) ".tex")) ; remove generated tex file 
        ))
    ))


(provide 'cra)

