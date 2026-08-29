;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold most-positive-fixnum)

(require 'use-package)

;; (load-file "~/.emacs.d/early-init.el")

(eval-after-load 'org
  (org-babel-load-file "~/.emacs.d/emacs26.org"))

(eval-after-load 'org
  (org-babel-load-file "~/.emacs.d/orgmode.org"))

(eval-after-load 'org
  (org-babel-load-file "~/.emacs.d/misc.org"))


;; Loading my own libraries
;; (add-to-list 'load-path (expand-file-name "mylibs" "~/.emacs.d"))

;; (use-package coingecko)
(setq my-libraries
      '("coingecko"
        "cra"
        "rb-read"
        ;; "mina"
        "my_magit_forge"
        "mygithub"
        "invoice"
        "loopless"
        "org-rg"
        "just"
        "nixcfg"
        "garmin"
        "light"
        "autorandr"
        "vol"
        "ir"
        "prospect"
        "ox-whatsapp"))

(cl-loop
 for lib in my-libraries
 do
 (let ((library (expand-file-name lib "~/.emacs.d/mylibs") ))
   (load-library library)
   (message "Loaded library %s" library)))
       

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 100 100))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval add-hook 'tuareg-mode-hook
	   (lambda nil
	     (setq-local comment-continue " ")))
     (projectile-project-test-cmd . "make test")
     (projectile-project-compilation-cmd . "dune build src/app/cli/src/mina.exe --profile=dev")))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'list-timers 'disabled nil)
