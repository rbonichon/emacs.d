;;; prospect.el --- Freelance prospect notes + tracker automation -*- lexical-binding: t; -*-

;; Mimics the manual workflow used when a job description / lead lands:
;;   1. paste the JD (region or clipboard),
;;   2. create ~/documents/org/zknotes/<timestamp>-prospect_<slug>.org with the
;;      standard skeleton (verdict, qui/quoi, JD, fit, comp, gates, actions, pièges),
;;   3. append a row to the Pipeline table of the prospect tracker, linking the note,
;;   4. open the note at the point where thinking starts.
;;
;; Entry points:
;;   M-x prospect/new            create note + tracker row (region or clipboard = JD)
;;   M-x prospect/goto-tracker   jump to the tracker's Pipeline table
;;   M-x prospect/set-status     restamp the Statut cell of the row at point
;;   M-x prospect/set-next-action  same for the Next action cell

(require 'org)
(require 'org-table)
(require 'org-id)
(require 'cl-lib)
(require 'seq)

(defgroup prospect nil
  "Freelance prospect tracking helpers."
  :group 'org)

(defcustom prospect/zknotes-dir (expand-file-name "~/documents/org/zknotes/")
  "Directory holding org-roam notes."
  :type 'directory)

(defcustom prospect/tracker-id "418b9117-7220-44de-b18d-b498d35b6e2c"
  "Org ID of the freelance prospect tracker note."
  :type 'string)

(defcustom prospect/tracker-file
  (expand-file-name "20260624070900-freelance_prospect_tracker.org"
                    "~/documents/org/zknotes/")
  "Fallback tracker path, used when `prospect/tracker-id' does not resolve."
  :type 'file)

(defcustom prospect/pipeline-heading "Pipeline"
  "Heading in the tracker under which the pipeline table lives."
  :type 'string)

(defcustom prospect/filetags ":perso:freelance:eurl:crypto:"
  "Default filetags for a new prospect note."
  :type 'string)

(defcustom prospect/default-status "🟡 P3 — loggé, pas de démarche"
  "Default value of the Statut column for a fresh prospect."
  :type 'string)

;;; ---------------------------------------------------------------- helpers

(defun prospect--slug (s)
  "Turn S into a filename slug: lowercase, underscores, no accents."
  (let* ((ascii (string-trim (downcase (or s ""))))
         (ascii (replace-regexp-in-string "[àâä]" "a" ascii))
         (ascii (replace-regexp-in-string "[éèêë]" "e" ascii))
         (ascii (replace-regexp-in-string "[îï]" "i" ascii))
         (ascii (replace-regexp-in-string "[ôö]" "o" ascii))
         (ascii (replace-regexp-in-string "[ùûü]" "u" ascii))
         (ascii (replace-regexp-in-string "ç" "c" ascii))
         (ascii (replace-regexp-in-string "[^a-z0-9]+" "_" ascii)))
    (string-trim ascii "_+" "_+")))

(defun prospect--jd-text ()
  "Return the job description text: the region if active, else the clipboard."
  (string-trim
   (if (use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
     (or (ignore-errors (current-kill 0 t)) ""))))

(defun prospect--tracker-file ()
  "Return the tracker file.
Use `prospect/tracker-file' when it exists (fast path); otherwise resolve
`prospect/tracker-id' with `org-id-find', which may trigger an ID scan."
  (if (file-exists-p prospect/tracker-file)
      prospect/tracker-file
    (or (car (ignore-errors (org-id-find prospect/tracker-id)))
        (user-error "Tracker not found (id %s)" prospect/tracker-id))))

(defun prospect--goto-pipeline-table ()
  "Move point to the first line of the pipeline table.  Assumes tracker buffer."
  (goto-char (point-min))
  (unless (re-search-forward
           (format "^\\*+[ \t]+%s[ \t]*$" (regexp-quote prospect/pipeline-heading))
           nil t)
    (user-error "No `%s' heading in %s" prospect/pipeline-heading buffer-file-name))
  (unless (re-search-forward "^[ \t]*|" nil t)
    (user-error "No table under `%s'" prospect/pipeline-heading))
  (beginning-of-line))

(defun prospect--append-row (row)
  "Append ROW (a full \"| … |\" line) to the table at point, then realign."
  (goto-char (org-table-end))
  (forward-line -1)
  (end-of-line)
  (insert "\n" row)
  (org-table-align))

(defun prospect--column (name)
  "Return the 1-based index of the column whose header matches NAME."
  (let* ((rows (seq-filter #'listp (org-table-to-lisp)))
         (header (car rows))
         (idx (cl-position-if (lambda (f) (string-match-p (regexp-quote name) f))
                              header)))
    (1+ (or idx (user-error "No column matching `%s'" name)))))

(defun prospect--stamp (text)
  "Prefix TEXT with today's day/month, the convention used in the tracker."
  (format "%s (%s)" text (format-time-string "%d/%m")))

;;; ---------------------------------------------------------------- skeleton

(defun prospect--skeleton (id company role source jd)
  "Return the org content of a fresh prospect note."
  (concat
   ":PROPERTIES:\n"
   (format ":ID:       %s\n" id)
   ":END:\n"
   (format "#+title: Prospect — %s%s\n"
           company
           (cond ((string-empty-p role) "")
                 ;; avoid "Company (Role (detail))" when the role is already parenthesised
                 ((string-match-p "(" role) (format " — %s" role))
                 (t (format " (%s)" role))))
   (format "#+filetags: %s\n" prospect/filetags)
   (format "#+date: [%s]\n\n" (format-time-string "%Y-%m-%d"))
   (format "Lié à [[id:%s][Prospect tracker]]. *JD reçue %s*%s\n\n"
           prospect/tracker-id
           (format-time-string "%Y-%m-%d")
           (if (string-empty-p source) "." (format " — %s" source)))
   "* Lecture rapide (verdict)\n\n"
   "TODO priorité (P1/P2/P3) + la raison en une phrase.\n\n"
   "* Qui / quoi\n\n"
   "- Produit / stack :\n"
   "- Financement (montant, date, lead) → *solvable ?* :\n"
   "- Sources :\n\n"
   "* La JD\n\n"
   "#+begin_quote\n"
   (string-trim jd) "\n"
   "#+end_quote\n\n"
   "* Fit\n\n"
   "| Axe (JD) | Verdict |\n"
   "|----------+---------|\n"
   "|          |         |\n\n"
   "→ verdict : combien de must-haves solides, et *est-ce que le rôle paie le pic\n"
   "de rareté (FM/vérif) ou seulement le tier « senior Rust » ?*\n\n"
   "* Structure & comp (estimations)\n\n"
   "| Scénario | Fourchette | Commentaire |\n"
   "|----------+------------+-------------|\n"
   "| *Contrat EURL (viser)* | $1 500-2 000/j (≈ 1 300-1 800 €/j) | vs *800 €/j* Repyh |\n"
   "| Presta cadrée (audit/revue) | forfait N j | productisable |\n"
   "| FT (EOR) + tokens | | met l'EURL en sommeil |\n\n"
   "* Gates à lever (dans l'ordre)\n\n"
   "1. 🔴 *§7.4 non-concurrence Repyh* — concurrent (blockchain L1 / crypto) ?\n"
   "   directement *ou via l'EURL*. Options : accord écrit / fin de contrat / park.\n"
   "2. 🟠 *Structure* : presta B2B possible, ou FT/EOR only ?\n"
   "3. 🟡 *Gaps must-have* : script prêt pour chaque gap.\n\n"
   "* Actions\n\n"
   "- [ ] intro chaude ? (15 min max de recherche)\n"
   "- [ ] \n\n"
   "* Pièges\n\n"
   "- *Sous-pricing par cadrage* : refuser le cadre « dev senior généraliste ».\n"
   "- *Concentration* : Repyh = 100 % du CA tant que le 2ᵉ client n'est pas signé.\n"
   "- Fiscalité USD/tokens → /EC ; placement du surplus → /investisseur.\n"))

;;; ---------------------------------------------------------------- commands

;;;###autoload
(defun prospect/new (company role source)
  "Create a prospect note for COMPANY / ROLE and log it in the tracker.
SOURCE is a free-form origin (\"email jobs@…\", URL, \"intro X\").
The job description is taken from the active region, or from the clipboard."
  (interactive
   (list (read-string "Company: ")
         (read-string "Role: ")
         (read-string "Source (email/URL/intro): ")))
  (let* ((jd (prospect--jd-text))
         (id (org-id-new))
         (file (expand-file-name
                (format "%s-prospect_%s.org"
                        (format-time-string "%Y%m%d%H%M%S")
                        (prospect--slug (concat company " " role)))
                prospect/zknotes-dir)))
    (when (string-empty-p jd)
      (message "prospect: no region and empty clipboard — JD block left empty"))
    (with-temp-file file
      (insert (prospect--skeleton id company role source jd)))
    (prospect/add-tracker-row company role id)
    (when (fboundp 'org-roam-db-update-file)
      (ignore-errors (org-roam-db-update-file file)))
    (find-file file)
    (goto-char (point-min))
    (re-search-forward "^\\* Lecture rapide" nil t)
    (message "prospect: %s created and logged in the tracker" (file-name-nondirectory file))
    file))

;;;###autoload
(defun prospect/add-tracker-row (company role note-id)
  "Append a pipeline row for COMPANY / ROLE linking NOTE-ID, in the tracker."
  (interactive (list (read-string "Company: ")
                     (read-string "Role: ")
                     (read-string "Note ID: ")))
  (let ((tracker (prospect--tracker-file)))
    (with-current-buffer (find-file-noselect tracker)
      (save-excursion
        (prospect--goto-pipeline-table)
        (prospect--append-row
         (format "| *%s*%s | | | | | %s → [[id:%s][note]] | à qualifier |"
                 company
                 (if (string-empty-p role) "" (format " — %s" role))
                 (prospect--stamp prospect/default-status)
                 note-id)))
      (save-buffer))))

;;;###autoload
(defun prospect/goto-tracker ()
  "Open the prospect tracker at its pipeline table."
  (interactive)
  (find-file (prospect--tracker-file))
  (prospect--goto-pipeline-table))

(defun prospect--set-cell (column prompt)
  "Set COLUMN of the tracker row at point, reading a value with PROMPT."
  (unless (org-at-table-p)
    (user-error "Point is not in a table row"))
  (let* ((col (prospect--column column))
         (current (string-trim (org-table-get nil col)))
         (value (read-string prompt current)))
    (org-table-put nil col (prospect--stamp value) t)
    (message "prospect: %s ← %s" column value)))

;;;###autoload
(defun prospect/set-status ()
  "Restamp the Statut cell of the tracker row at point (date-stamped)."
  (interactive)
  (prospect--set-cell "Statut" "Statut: "))

;;;###autoload
(defun prospect/set-next-action ()
  "Restamp the Next action cell of the tracker row at point."
  (interactive)
  (prospect--set-cell "Next action" "Next action: "))

(provide 'prospect)
;;; prospect.el ends here
