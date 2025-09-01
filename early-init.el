;;(load-file "~/.emacs.d/rb-theme.el")

;; Emacs configuration file
;; Generic options
;; Do not write anything at startup
(setq inhibit-startup-screen t)
(setq debug-on-error t)

;; (setq exec-path (append '("/home/richard/devel/Pygments-1.6") exec-path ))

(setq home-emacs-dir (concat (getenv "HOME") "/.emacs.d/"))

;; The rest of the init file.

;; don't let Customize mess with my .emacs
;; (setq custom-file (concat home-emacs-dir "custom.el"))
;; (load custom-file 'noerror)

; Don not put line numbers
;; (global-linum-mode -1)

;; Display line and column please
(column-number-mode t)
;;(line-number-mode t)
(setq global-hl-mode t)

(setq initial-scratch-message "") ;; Uh, I know what Scratch is for

(setq visible-bell t)             ;; Get rid of the beeps

;;No bars please
(when
    (and (window-system) (fboundp 'horizontal-scroll-bar-mode))
  (horizontal-scroll-bar-mode -1))
(scroll-bar-mode -1)
(tool-bar-mode 0)
(menu-bar-mode -99)


;;Set frame title
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs : %b"))
