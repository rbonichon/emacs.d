;;; vacances-scolaires.el --- Calendrier scolaire français pour l'agenda org -*- lexical-binding: t; -*-

;; Author: Richard
;; Keywords: calendar, org
;; Version: 0.1.0

;;; Commentary:

;; Prédicats de date destinés aux timestamps sexp d'org, pour les activités
;; hebdomadaires qui suivent le calendrier scolaire (piscine, danse, etc.) :
;; elles ont lieu toutes les semaines *sauf* pendant les vacances et les jours
;; fériés.
;;
;; Dans un fichier org :
;;
;;   *** Piscine Sophie
;;   <%%(vacances-scolaires-activite-p date 5 '(9 18 2026)) 17:00-18:00>
;;
;; soit « tous les vendredis (5) d'école à partir du 18 septembre 2026 ».
;;
;; Le calendrier officiel n'est publié que quelques années à l'avance, donc
;; `vacances-scolaires-periodes' ne couvre qu'une plage bornée
;; (`vacances-scolaires-couverture').  Hors de cette plage les prédicats
;; renvoient nil : l'activité disparaît de l'agenda au lieu de se répéter à
;; l'infini sur des dates non vérifiées.  Quand une nouvelle année scolaire est
;; publiée, ajouter ses périodes et étendre la couverture.

;;; Code:

(require 'calendar)
(require 'cl-lib)

(defgroup vacances-scolaires nil
  "Calendrier scolaire français."
  :group 'calendar)

(defcustom vacances-scolaires-periodes
  ;; Zone C (Toulouse) — calendrier officiel 2026-2027.
  ;; Chaque période est (DÉBUT REPRISE) au format calendrier (MOIS JOUR ANNÉE),
  ;; recopié tel quel du calendrier officiel : les vacances courent de DÉBUT
  ;; inclus à REPRISE exclu, la reprise des cours étant le jour REPRISE.
  '(((10 17 2026) (11 2 2026))   ; Toussaint
    ((12 19 2026) (1 4 2027))    ; Noël
    ((2 6 2027)   (2 22 2027))   ; Hiver
    ((4 3 2027)   (4 19 2027))   ; Printemps
    ((7 3 2027)   (9 1 2027)))   ; Été (rentrée 2027 non encore publiée)
  "Périodes de vacances scolaires, sous forme de couples (DÉBUT REPRISE)."
  :type '(repeat (list (list integer integer integer)
                       (list integer integer integer)))
  :group 'vacances-scolaires)

(defcustom vacances-scolaires-couverture '((9 1 2026) (9 1 2027))
  "Plage (DÉBUT FIN) réellement couverte par `vacances-scolaires-periodes'.
Bornes incluses, au format calendrier (MOIS JOUR ANNÉE)."
  :type '(list (list integer integer integer) (list integer integer integer))
  :group 'vacances-scolaires)

(defcustom vacances-scolaires-pont-ascension t
  "Non-nil si le vendredi suivant l'Ascension est chômé (pont)."
  :type 'boolean
  :group 'vacances-scolaires)

(defun vacances-scolaires--abs (date)
  "Numéro de jour absolu de DATE au format calendrier (MOIS JOUR ANNÉE)."
  (calendar-absolute-from-gregorian date))

(defun vacances-scolaires-couverte-p (date)
  "Non-nil si DATE est dans la plage couverte par le calendrier connu."
  (let ((jour (vacances-scolaires--abs date)))
    (and (>= jour (vacances-scolaires--abs (nth 0 vacances-scolaires-couverture)))
         (<= jour (vacances-scolaires--abs (nth 1 vacances-scolaires-couverture))))))

(defun vacances-scolaires-p (date)
  "Non-nil si DATE tombe pendant les vacances scolaires."
  (let ((jour (vacances-scolaires--abs date)))
    (cl-some (lambda (periode)
               (and (>= jour (vacances-scolaires--abs (nth 0 periode)))
                    (<  jour (vacances-scolaires--abs (nth 1 periode)))))
             vacances-scolaires-periodes)))

(defun vacances-scolaires--paques (annee)
  "Jour absolu du dimanche de Pâques de l'ANNEE grégorienne.
Algorithme dit de Meeus/Butcher ; Emacs 30 ne fournit pas de fonction
équivalente, `holiday-easter-etc' calculant Pâques en interne."
  (let* ((a (% annee 19))
         (b (/ annee 100))
         (c (% annee 100))
         (d (/ b 4))
         (e (% b 4))
         (f (/ (+ b 8) 25))
         (g (/ (+ (- b f) 1) 3))
         (h (% (+ (* 19 a) (- b d g) 15) 30))
         (i (/ c 4))
         (k (% c 4))
         (l (% (- (+ 32 (* 2 e) (* 2 i)) h k) 7))
         (m (/ (+ a (* 11 h) (* 22 l)) 451))
         (n (+ h l (- (* 7 m)) 114)))
    (calendar-absolute-from-gregorian (list (/ n 31) (1+ (% n 31)) annee))))

(defun vacances-scolaires-feries (annee)
  "Liste des jours fériés (en jours absolus) de l'ANNEE civile."
  (let ((paques (vacances-scolaires--paques annee)))
    (append (mapcar (lambda (md)
                      (vacances-scolaires--abs (list (car md) (cadr md) annee)))
                    '((1 1) (5 1) (5 8) (7 14) (8 15) (11 1) (11 11) (12 25)))
            (list (+ paques 1)    ; lundi de Pâques
                  (+ paques 39)   ; Ascension
                  (+ paques 50))  ; lundi de Pentecôte
            (when vacances-scolaires-pont-ascension
              (list (+ paques 40))))))

(defun vacances-scolaires-ferie-p (date)
  "Non-nil si DATE est un jour férié (ou le pont de l'Ascension)."
  (memq (vacances-scolaires--abs date)
        (vacances-scolaires-feries (calendar-extract-year date))))

(defun vacances-scolaires-jour-ecole-p (date)
  "Non-nil si DATE est en période scolaire : ni vacances, ni férié.
Renvoie nil hors de la plage couverte, cf. `vacances-scolaires-couverture'.
Les samedis et dimanches ne sont pas exclus : les activités périscolaires
suivent le calendrier scolaire sans être des jours de classe."
  (and (vacances-scolaires-couverte-p date)
       (not (vacances-scolaires-p date))
       (not (vacances-scolaires-ferie-p date))))

(defun vacances-scolaires-activite-p (date jour &optional debut fin)
  "Non-nil si DATE porte une activité hebdomadaire en période scolaire.
JOUR est le jour de la semaine au sens de `calendar-day-of-week'
\(0 = dimanche, 5 = vendredi, 6 = samedi).  DEBUT et FIN, optionnels et au
format calendrier (MOIS JOUR ANNÉE), bornent l'activité, bornes incluses."
  (let ((abs (vacances-scolaires--abs date)))
    (and (= jour (calendar-day-of-week date))
         (or (null debut) (>= abs (vacances-scolaires--abs debut)))
         (or (null fin)   (<= abs (vacances-scolaires--abs fin)))
         (vacances-scolaires-jour-ecole-p date))))

(provide 'vacances-scolaires)
;;; vacances-scolaires.el ends here
