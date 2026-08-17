;;; nixcfg.el --- Drive the ~/nix-config flake from anywhere -*- lexical-binding: t; -*-

;; Author: Richard Bonichon
;; Keywords: tools, convenience, nix

;;; Commentary:
;; Mirrors the recipes of ~/nix-config/justfile, but runs the underlying
;; commands directly (no `just' in between):
;;
;;   update  ->      nix flake update [INPUTS...]
;;   eval    ->      nix eval --raw .#nixosConfigurations.HOST.config.system.build.toplevel.drvPath
;;   build   ->      nixos-rebuild build  --flake .#HOST
;;   switch  -> sudo nixos-rebuild switch --flake .#HOST
;;
;; HOST defaults to this machine (`nixcfg-current-host'), and every command runs
;; with `default-directory' bound to `nixcfg-directory', so the entry points work
;; from any path.
;;
;; Interactively:
;;   M-x nixcfg-update    M-x nixcfg-build    M-x nixcfg-switch    M-x nixcfg-eval
;; A prefix argument prompts for what would otherwise be defaulted (the host for
;; the rebuild commands, the inputs for update).
;;
;; In eshell (any directory):
;;   $ nixos-update                 # nix flake update
;;   $ nixos-update nixpkgs home-manager
;;   $ nixos-build                  # this host
;;   $ nixos-build slimbook         # another host
;;   $ nixos-switch
;;   $ nixup / nixbuild / nixswitch # short aliases
;;
;; Output lands in a comint-enabled compilation buffer (*nixcfg switch* &c.), so
;; `sudo' can ask for its password and C-x ` jumps to Nix errors.

(require 'compile)
(require 'seq)
(require 'subr-x)

(defgroup nixcfg nil
  "Run the NixOS flake commands of ~/nix-config."
  :group 'tools)

(defcustom nixcfg-directory "~/nix-config"
  "Directory holding the NixOS flake."
  :type 'directory)

(defcustom nixcfg-host nil
  "Host name to use by default.
When nil, `nixcfg-current-host' derives it from the running machine."
  :type '(choice (const :tag "Current machine" nil) string))

(defcustom nixcfg-nixos-rebuild "nixos-rebuild"
  "Program used to build/activate a system closure."
  :type 'string)

(defcustom nixcfg-nix "nix"
  "Program used for flake operations (update, eval)."
  :type 'string)

(defcustom nixcfg-privileged-actions '("switch" "boot" "test" "dry-activate")
  "Rebuild actions that must run through `sudo'."
  :type '(repeat string))

(defconst nixcfg-rebuild-actions
  '("build" "switch" "boot" "test" "dry-build" "dry-activate" "build-vm")
  "Actions accepted by `nixcfg-rebuild'.")

;;; Locating things

(defun nixcfg--dir ()
  "Return `nixcfg-directory' as an existing absolute directory name."
  (let ((dir (file-name-as-directory (expand-file-name nixcfg-directory))))
    (unless (file-directory-p dir)
      (user-error "No such nix configuration directory: %s" dir))
    dir))

(defun nixcfg-current-host ()
  "Return the host name the flake commands default to.
`nixcfg-host' when set, otherwise the short name of this machine (what
the justfile computes with `hostname')."
  (or nixcfg-host
      (car (split-string (system-name) "\\." t))))

(defun nixcfg-hosts ()
  "Return the host names declared under `nixcfg-directory'/hosts."
  (let ((hosts (expand-file-name "hosts" (nixcfg--dir))))
    (when (file-directory-p hosts)
      (seq-filter (lambda (name)
                    (file-directory-p (expand-file-name name hosts)))
                  (directory-files hosts nil directory-files-no-dot-files-regexp)))))

(defun nixcfg-inputs ()
  "Return the flake's direct inputs, read from flake.lock."
  (let ((lock (expand-file-name "flake.lock" (nixcfg--dir))))
    (when (file-readable-p lock)
      (with-temp-buffer
        (insert-file-contents lock)
        (let* ((json (json-parse-buffer :object-type 'alist))
               (root (alist-get 'root (alist-get 'nodes json)))
               (inputs (alist-get 'inputs root)))
          (mapcar (lambda (cell) (symbol-name (car cell))) inputs))))))

;;; Reading arguments

(defun nixcfg-read-host ()
  "Prompt for a host, defaulting to `nixcfg-current-host'."
  (let ((default (nixcfg-current-host)))
    (completing-read (format "Host (default %s): " default)
                     (nixcfg-hosts) nil nil nil nil default)))

(defun nixcfg-read-inputs ()
  "Prompt for flake inputs to update; empty answer means all of them."
  (completing-read-multiple
   "Inputs to update (empty = all): " (nixcfg-inputs) nil nil))

(defun nixcfg--arg-host ()
  "Host argument for an interactive spec: prompt only under a prefix argument."
  (when current-prefix-arg (nixcfg-read-host)))

;;; Running

(defun nixcfg-run (label command)
  "Run COMMAND in `nixcfg-directory', displaying it in a buffer named after LABEL.
The buffer runs `comint-mode' so that interactive prompts (typically
`sudo' asking for a password) work.  Returns the compilation buffer."
  (let ((default-directory (nixcfg--dir)))
    (message "nixcfg: %s (in %s)" command default-directory)
    (compilation-start command t
                       (lambda (_mode) (format "*nixcfg %s*" label)))))

(defun nixcfg-flake-ref (&optional host)
  "Return the flake reference for HOST (`nixcfg-current-host' when nil).
Relative to `nixcfg-directory', which is where commands are run."
  (format ".#%s" (or host (nixcfg-current-host))))

;;; Commands

;;;###autoload
(defun nixcfg-update (&optional inputs)
  "Update the flake inputs of `nixcfg-directory'.
INPUTS is a list of input names; nil updates every input.
Interactively, a prefix argument prompts for the inputs to update."
  (interactive (list (when current-prefix-arg (nixcfg-read-inputs))))
  (nixcfg-run "update"
              (string-join (cons (format "%s flake update" nixcfg-nix)
                                 (mapcar #'shell-quote-argument inputs))
                           " ")))

;;;###autoload
(defun nixcfg-rebuild (action &optional host)
  "Run ACTION of `nixos-rebuild' on HOST's configuration.
ACTION is one of `nixcfg-rebuild-actions'; those listed in
`nixcfg-privileged-actions' are run under `sudo'.  HOST defaults to
`nixcfg-current-host'.  Interactively, prompt for both."
  (interactive (list (completing-read "Action: " nixcfg-rebuild-actions nil t)
                     (nixcfg-read-host)))
  (nixcfg-run action
              (format "%s%s %s --flake %s"
                      (if (member action nixcfg-privileged-actions) "sudo " "")
                      nixcfg-nixos-rebuild
                      action
                      (shell-quote-argument (nixcfg-flake-ref host)))))

;;;###autoload
(defun nixcfg-build (&optional host)
  "Build HOST's system closure (result symlink in `nixcfg-directory').
HOST defaults to `nixcfg-current-host'; a prefix argument prompts for it."
  (interactive (list (nixcfg--arg-host)))
  (nixcfg-rebuild "build" host))

;;;###autoload
(defun nixcfg-switch (&optional host)
  "Build and activate HOST's configuration (through `sudo').
HOST defaults to `nixcfg-current-host'; a prefix argument prompts for it."
  (interactive (list (nixcfg--arg-host)))
  (nixcfg-rebuild "switch" host))

;;;###autoload
(defun nixcfg-eval (&optional host)
  "Evaluate HOST's system derivation without building it.
HOST defaults to `nixcfg-current-host'; a prefix argument prompts for it."
  (interactive (list (nixcfg--arg-host)))
  (nixcfg-run "eval"
              (format "%s eval --raw %s"
                      nixcfg-nix
                      (shell-quote-argument
                       (format ".#nixosConfigurations.%s.config.system.build.toplevel.drvPath"
                               (or host (nixcfg-current-host)))))))

;;; Eshell commands
;; Defined as `eshell/NAME' functions rather than aliases so they are available
;; in every eshell, whatever the current directory, and take a host argument.

(defun eshell/nixos-update (&rest inputs)
  "Eshell command: update the flake inputs, all of them when INPUTS is empty."
  (nixcfg-update (mapcar (lambda (i) (format "%s" i)) inputs))
  nil)

(defun eshell/nixos-build (&optional host &rest _ignored)
  "Eshell command: build HOST's closure (defaults to this machine)."
  (nixcfg-build (and host (format "%s" host)))
  nil)

(defun eshell/nixos-switch (&optional host &rest _ignored)
  "Eshell command: activate HOST's configuration (defaults to this machine)."
  (nixcfg-switch (and host (format "%s" host)))
  nil)

(defun eshell/nixos-eval (&optional host &rest _ignored)
  "Eshell command: evaluate HOST's system derivation (defaults to this machine)."
  (nixcfg-eval (and host (format "%s" host)))
  nil)

(defalias 'eshell/nixup #'eshell/nixos-update)
(defalias 'eshell/nixbuild #'eshell/nixos-build)
(defalias 'eshell/nixswitch #'eshell/nixos-switch)
(defalias 'eshell/nixeval #'eshell/nixos-eval)

(provide 'nixcfg)
;;; nixcfg.el ends here
