;;; ir.el --- Calcul de l'impôt sur le revenu -*- lexical-binding: t; -*-

(defgroup ir-calcul nil
  "Calculateur d'impôt sur le revenu (France)."
  :group 'applications)

(defcustom ir-bareme-seuils '(0 11294 28797 82341 177106)
  "Seuils des tranches d'imposition (bornes inférieures)."
  :type '(repeat integer)
  :group 'ir-calcul)

(defcustom ir-bareme-tauxs '(0 0.11 0.30 0.41 0.45)
  "Taux d'imposition correspondants aux tranches."
  :type '(repeat number)
  :group 'ir-calcul)

(defcustom ir-plafond-quotient-familial 1759
  "Plafond d'avantage fiscal par demi-part (en euros)."
  :type 'integer
  :group 'ir-calcul)

(defun calculer-ir (revenu parts &optional seuils tauxs plafond-avantage)
  "Calcule l'impôt sur le revenu pour REVENUE et PARTS fiscales.
Utilise `ir-bareme-seuils`, `ir-bareme-tauxs`, et `ir-plafond-quotient-familial`
sauf si des arguments personnalisés sont fournis."
  (let* ((seuils (or seuils ir-bareme-seuils))
         (tauxs  (or tauxs  ir-bareme-tauxs))
         (plafond-avantage (or plafond-avantage ir-plafond-quotient-familial))
         (quotient (/ revenu parts))
         (impot-par-part 0)
         (impot-total 0))

    ;; Impôt par part par tranche
    (cl-loop for i from 0 to (- (length seuils) 2) do
             (let* ((inf (nth i seuils))
                    (sup (nth (1+ i) seuils))
                    (taux (nth i tauxs)))
               (when (> quotient inf)
                 (setq impot-par-part
                       (+ impot-par-part (* (min (- quotient inf) (- sup inf)) taux))))))

    ;; Dernière tranche
    (let ((dernier-seuil (car (last seuils)))
          (dernier-taux (car (last tauxs))))
      (when (> quotient dernier-seuil)
        (setq impot-par-part
              (+ impot-par-part (* (- quotient dernier-seuil) dernier-taux)))))

    (setq impot-total (* impot-par-part parts))

    ;; Plafonnement du quotient familial
    (let* ((parts-base 2)
           (parts-sup (- parts parts-base)))
      (when (> parts-sup 0)
        (let ((quotient-ref (/ revenu parts-base))
              (impot-ref 0))

          ;; Référence : 2 parts
          (cl-loop for i from 0 to (- (length seuils) 2) do
                   (let* ((inf (nth i seuils))
                          (sup (nth (1+ i) seuils))
                          (taux (nth i tauxs)))
                     (when (> quotient-ref inf)
                       (setq impot-ref
                             (+ impot-ref (* (min (- quotient-ref inf) (- sup inf)) taux))))))

          (let ((dernier-seuil (car (last seuils)))
                (dernier-taux (car (last tauxs))))
            (when (> quotient-ref dernier-seuil)
              (setq impot-ref
                    (+ impot-ref (* (- quotient-ref dernier-seuil) dernier-taux)))))

          (setq impot-ref (* impot-ref parts-base))

          ;; Appliquer plafonnement
          (let* ((gain (- impot-ref impot-total))
                 (plafond (* plafond-avantage (* 2 parts-sup)))) ; demi-parts
            (when (> gain plafond)
              (setq impot-total (- impot-ref plafond)))))))

    (round impot-total)))

(provide 'ir)
;;; ir.el ends here
