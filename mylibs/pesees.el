;;; pesees.el --- Weigh-in log with 7-day rolling average -*- lexical-binding: t; -*-

;; Keeps the "Pesées" table of the weigh-in note:
;;
;;   | Date | Poids | Moy. 7 j | Δ 7 j | Note |
;;
;; Moy. 7 j is the mean of the weigh-ins dated within the 7 days ending on
;; the row's date (inclusive).  Δ 7 j is that mean minus the mean of the
;; latest weigh-in dated at least 7 days earlier (empty when there is none).
;;
;; Entry points:
;;   M-x pesees/add          add or replace a weigh-in, recompute the table
;;   M-x pesees/recompute    recompute the derived columns after a manual edit
;;   M-x pesees/goto         jump to the table

(require 'org)
(require 'org-table)
(require 'org-id)
(require 'cl-lib)
(require 'seq)

(defgroup pesees nil
  "Weigh-in log helpers."
  :group 'org)

(defcustom pesees/note-id "5c0e8a3d-2f71-4b6a-9d42-8e1f7a6b3c90"
  "Org ID of the weigh-in note."
  :type 'string)

(defcustom pesees/note-file
  (expand-file-name "20261009080000-suivi_pesees_2026.org" "~/documents/org/zknotes/")
  "Fallback note path, used when `pesees/note-id' does not resolve."
  :type 'file)

(defcustom pesees/heading "Pesées"
  "Heading under which the weigh-in table lives."
  :type 'string)

(defcustom pesees/window-days 7
  "Length of the rolling-average window, in days."
  :type 'integer)

(defun pesees/--file ()
  "File holding the weigh-in note."
  (or (ignore-errors (car (org-id-find pesees/note-id)))
      pesees/note-file))

(defun pesees/--goto-table ()
  "Move point to the first data row of the table under `pesees/heading'."
  (goto-char (point-min))
  (unless (re-search-forward
           (format "^\\*+[ \t]+%s[ \t]*$" (regexp-quote pesees/heading)) nil t)
    (user-error "No \"%s\" heading in %s" pesees/heading (buffer-file-name)))
  (unless (re-search-forward org-table-line-regexp nil t)
    (user-error "No table under \"%s\"" pesees/heading))
  (goto-char (org-table-begin)))

(defun pesees/--day (date)
  "Day number of DATE, an org timestamp or YYYY-MM-DD string."
  (time-to-days (org-time-string-to-time date)))

(defun pesees/--read-rows ()
  "Data rows of the table at point, as lists (DATE WEIGHT NOTE)."
  (cl-loop for row in (org-table-to-lisp)
           when (and (listp row)
                     (string-match-p "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" (nth 0 row)))
           collect (list (string-trim (nth 0 row))
                         (string-to-number (nth 1 row))
                         (string-trim (or (nth 4 row) "")))))

(defun pesees/--mean (rows day)
  "Mean weight of ROWS dated in the window ending on DAY."
  (let ((in (seq-filter (lambda (r)
                          (let ((d (pesees/--day (car r))))
                            (and (<= d day) (> d (- day pesees/window-days)))))
                        rows)))
    (/ (apply #'+ (mapcar #'cadr in)) (float (length in)))))

(defun pesees/--render (rows)
  "Table lines for ROWS, sorted by date, with derived columns."
  (let ((rows (sort (copy-sequence rows)
                    (lambda (a b) (< (pesees/--day (car a)) (pesees/--day (car b)))))))
    (mapcar
     (lambda (r)
       (let* ((day (pesees/--day (car r)))
              (mean (pesees/--mean rows day))
              (before (seq-filter (lambda (o)
                                    (<= (pesees/--day (car o))
                                        (- day pesees/window-days)))
                                  rows))
              (delta (when before
                       (- mean (pesees/--mean rows (pesees/--day (caar (last before))))))))
         (format "| %s | %.1f | %.2f | %s | %s |"
                 (car r) (cadr r) mean
                 (if delta (format "%+.2f" delta) "")
                 (nth 2 r))))
     rows)))

(defun pesees/--rewrite (rows)
  "Replace the data rows of the table at point by ROWS, recomputed."
  (let* ((beg (org-table-begin))
         (end (org-table-end))
         (header (save-excursion
                   (goto-char beg)
                   (buffer-substring beg (progn (forward-line 2) (point))))))
    (delete-region beg end)
    (goto-char beg)
    (insert header (string-join (pesees/--render rows) "\n") "\n")
    (goto-char beg)
    (org-table-align)))

(defun pesees/--timestamp (date)
  "Inactive org date stamp for DATE (YYYY-MM-DD)."
  (format-time-string "[%Y-%m-%d %a]" (org-time-string-to-time date)))

;;;###autoload
(defun pesees/add (weight &optional date note)
  "Record WEIGHT (kg) for DATE (default today), with an optional NOTE.
A weigh-in already recorded for DATE is replaced.  Saves the note and
returns the new 7-day mean."
  (interactive
   (list (read-number "Poids (kg) : ")
         (org-read-date nil nil nil "Date : ")
         (read-string "Note (facultatif) : ")))
  (let* ((date (or date (format-time-string "%Y-%m-%d")))
         (stamp (pesees/--timestamp date))
         (buffer (find-file-noselect (pesees/--file))))
    (with-current-buffer buffer
      (save-excursion
        (pesees/--goto-table)
        (let* ((rows (seq-remove (lambda (r) (= (pesees/--day (car r)) (pesees/--day date)))
                                 (pesees/--read-rows)))
               (rows (cons (list stamp (float weight) (or note "")) rows)))
          (pesees/--rewrite rows)
          (save-buffer)
          (let ((mean (pesees/--mean rows (pesees/--day date))))
            (message "Pesée %s : %.1f kg, moyenne %d j : %.2f kg"
                     stamp weight pesees/window-days mean)
            mean))))))

;;;###autoload
(defun pesees/recompute ()
  "Recompute the derived columns of the weigh-in table and save."
  (interactive)
  (with-current-buffer (find-file-noselect (pesees/--file))
    (save-excursion
      (pesees/--goto-table)
      (pesees/--rewrite (pesees/--read-rows))
      (save-buffer))))

;;;###autoload
(defun pesees/goto ()
  "Open the weigh-in note at its table."
  (interactive)
  (find-file (pesees/--file))
  (pesees/--goto-table))

(provide 'pesees)
;;; pesees.el ends here
