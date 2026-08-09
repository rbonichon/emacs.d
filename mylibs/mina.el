(require 'transient)
(require 'projectile)
(require 'dash)

(transient-define-prefix
 mina/dispatch
 ()
 "Show main Mina dispatch menu"
 ["Compilation" ("c" "Main project compilation" mina/compile-project)]
 ["Tests" ("t" "Run main test" mina/run-tests)])

(defun mina--relative-from-root (dunefile)
  "Get relative path from the project root directory"
  (string-remove-prefix (projectile-project-root) dunefile))

(defun mina/run-tests ()
  "docstring"
  (interactive)
  (message "not yet implemented"))

(defun mina/run-test (cmd)
  "docstring"
  (interactive)
  (projectile--run-project-cmd
   cmd
   projectile-test-cmd-map
   :show-prompt nil
   :prompt-prefix "Test: "
   :save-buffers t))


(defun mina--get-issue-number (name)
  (save-match-data
    (let* ((regexp (rx "#" (group (1+ digit))))
           (beg (string-match regexp name)))
      (when beg (match-string 1 name)))))

(defun mina--get-issue-from-branch ()
  (mina--get-issue-number
   (magit-get-current-branch)))

(defun mina--view-issue-node ()
  (interactive)
  (let ((branch
         (magit-get-current-branch))
        (issue
         (mina--get-issue-from-branch)))
    (when issue
      (let* ((title (concat "Issue " issue))
             (node
              (or (org-roam-node-from-title-or-alias
                   title)
                  (org-roam-node-create
                   :title title))))
        (if (org-roam-node-file node)
            (org-roam-node-visit node other-window)
          (org-roam-capture-
           :node node
           :props '(:finalize find-file)))))))

(defun mina-find-invocation ()
  "Find the command in the invocation part of the test file"
  (let ((invocation-text "Invocation:"))
    (save-excursion
      (beginning-of-buffer)
      (when (search-forward
             invocation-text
             nil
             :noerror)
        (let ((continue t)
              (lines))
          (while continue
            (let ((text (string-trim
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))))
              (message
               (format "Got %s" text))
              (unless (string-equal
                       (substring text -1 nil)
                       "\\")
                (setq continue nil))
              (push text lines))
            (next-line)
            (message "once more"))
          (let ((invocation (string-clean-whitespace
                             (string-replace
                              invocation-text
                              ""
                              (string-replace
                               "\\"
                               " "
                               (eval
                                (cons 'concat (reverse lines))))))))
            (message
             (format
              "invocation is: %s"
              invocation))
            invocation))))))

(defun mina/run-invocation ()
  "Run command based on invocation string as documented in current buffer."
  (interactive)
  (let ((cmd (mina-find-invocation)))
    (projectile--run-project-cmd
     cmd
     projectile-test-cmd-map
     :show-prompt nil
     :prompt-prefix "Test: "
     :save-buffers t
     )
    )
  )

(defun mina-timestamp ()
  (format-time-string "%Y-%m-%dT%H:%M:%S")
  )

(defun mina-run-process (cmd buffer-name &rest args)
  "Run CMD in buffer BUFFER-NAME."
  (let ((buf (get-buffer-create buffer-name))
        ;; derive name from command using the first word
        (procname cmd))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format  "* Command \"%s\" started at %s *\n\n" cmd (mina-timestamp)))
      )
    (start-process procname buffer-name cmd)
    (switch-to-buffer buf)
    )
  )

(defun mina/run-build (cmd)
  (projectile--run-project-cmd
   cmd
   projectile-compilation-cmd-map
   :show-prompt nil
   :prompt-prefix "Build: "
   :save-buffers t))

(setq mina/build-targets
      '(("mina" . "src/app/cli/src/mina.exe")
        ("logproc" . "src/app/logproc/logproc.exe")
        ("archive" .  "src/app/archive/archive.exe")
        ("replayer" . "src/app/replayer/replayer.exe")
        ("dump_blocks" . "src/app/dump_blocks/dump_blocks")))

(defun mina--get-build-target (target-key)
  "docstring"
  (cdr (assoc target-key mina/build-targets))
  )

(defun mina--select-build-targets ()
  "Select among some of the predefined build targets as
determined by variable MINA/BUILD-TARGETS."
  (let ((choice (completing-read
                 "build-target:"
                 mina/build-targets)))
    (mina--get-build-target choice)))

(defun mina/dune-build (specs)
  "Invoke dune build on a given target using SPECS.

SPECS is a plist containing
- :profile (default dev)
- :target (default src/app/cli/src/mina.exe)
- :display (default quiet)
- :cache   (default enabled)
"
  (interactive)
  (let* ((profile (or (plist-get specs :profile) 'dev))
         (target (or (mina--get-build-target
                      (plist-get specs :target))
                     (mina--select-build-targets)))
         (display (or (plist-get specs :display)
                      'quiet))
         (cache   (or (plist-get specs :cache)
                      'enabled))
         (cmd
          (format "dune build %s --profile=%s --display=%s --cache=%s"
                  target
                  profile
                  display
                  cache))
         (buffer-name "*mina build*"))
    (mina/run-build cmd)))


(defun mina--get-tests-of-buffer (&optional bufname)
  "docstring"
  (interactive)
  (let ((l)
        (names))
    (with-current-buffer (or bufname (current-buffer))
      (save-excursion
        (goto-char (point-min))
        ;; Construct the list of positions that seem to have a definition of a test
        (while (search-forward "let%test" nil :noerror)
          (let ((line (line-number-at-pos (point))))
            ;; The name of the test is the next string
            (search-forward "\"")
            (setq beg (point))
            (search-forward "\"")
            (backward-char 1)
            (let ((name (buffer-substring-no-properties beg (point))))
              ;; The name should be unique to be used with (completing-read)
              (push (list (format "%s[%d]" name line) line) names))))))
    (message "%s" names)
    names))

(defun mina/run-1-test-for-current-buffer ()
  "Select a unit test to be run from the current opened file."
  (interactive)
  (let* ((names (mina--get-tests-of-buffer))
         (choice (completing-read "Run test: " names))
         (line (cadr (assoc choice names)))
         (file (buffer-file-name (current-buffer))))
    ;; (message "%s %s %s" names file line)
    (let ((cmd (format "./scripts/testone.sh %s %d" file line)))
      (message cmd)
      (when line
        (projectile-run-async-shell-command-in-root cmd)))))

(defun mina--get-current-libpath ()
  "Retrieve current directory (library) path from root"
  (let* ((root (projectile-project-root))
         (dunefile (buffer-file-name (current-buffer)))
         (dirname (file-name-directory dunefile))
         (libpath (mina--relative-from-root dirname)))
    libpath))

(defun mina/build-current-library ()
  "Run command dune build /path/from/root/to/current/directory"
  (interactive)
  (let ((libpath (mina--get-current-libpath))
        (display 'short))               ; Use short display to compile library
    (mina/dune-build `(:target ,libpath :display ,display))))

(defun mina/run-test-of-current-directory ()
  "Run command 'dune runtest /path/from/root/to/current/directory'"
  (interactive)
  (let ((libpath (mina--get-current-libpath)))
    (mina/run-test (format "dune runtest %s" libpath))))

(transient-define-prefix
 mina/compile-project () "Show different compilation targets"
 ["Compilation"
  ("a" "Build archives"
   (lambda ()
     (interactive)
     (mina/dune-build '(:target "archive"))))
    ("b"
     "Build (generic)"
     (lambda ()
       (interactive)
       (mina/dune-build '())))
    ("d" "Compile directory"
   (lambda ()
     (interactive)
     (mina/build-current-library)))
  ("l" "Build logproc.exe"
   (lambda ()
     (interactive)
     (mina/dune-build '(:target 'logproc))))
  ("m" "Build mina.exe"
   (lambda ()
     (interactive)
     (mina/dune-build '(:target 'mina)))
   )
  ("s" "Build signatures"
   (lambda ()
     (interactive)
     (mina/dune-build
      `(:target
        ,(mapconcat
          (lambda (it) (format "src/app/cli/src/mina_%s_signatures.exe" it))
          (list "testnet" "mainnet")
          " ")))))]
 ["Test"
  ("u" "Run single test for this file" mina/run-1-test-for-current-buffer)
  ("t" "Run tests for current directory" mina/run-test-of-current-directory)])


;; Specific org facilities
(require 'org)

;;
(defmacro ghlinks (org-link-name repository)
  `(progn
     (org-link-set-parameters
      ,(format "%s!" org-link-name)
      :face '(:weight bold :underline t :foreground "linkColor")
      :follow (lambda (path)
                (shell-command
                 (format
                  "xdg-open https://github.com/%s/pull/%s"
                  ,repository
                  path))))
     (org-link-set-parameters
      ,(format "%s#" org-link-name)
      :face '(:weight bold :underline t :foreground "linkColor")
      :follow (lambda (path)
                (shell-command
                 (format
                  "xdg-open https://github.com/%s/issues/%s"
                  ,repository
                  path)))))
  )


(ghlinks "snarkyjs" "o1-labs/snarkyjs-bindings")
(ghlinks "proof-systems" "o1-labs/proof-systems")

(defun mina-run-merkle-tests ()
  "docstring"
  (interactive)
  (let ((cmd
         "dune runtest --instrument-with bisect_ppx --force src/lib/merkle_ledger_tests/ && ../scripts/mask_coverage.sh"))
    (projectile-run-shell-command-in-root
     cmd
     "*mina-bisect-ppx*")))

(defun mina-get-coverage ()
  "docstring"
  (interactive)
  (save-current-buffer
    (let* ((pos (progn (newline) (point)))
           (dunefile "/Users/richard/documents/org/zknotes/20240201103617-coverage_of_merkle_mask.org")
           (other-buffer (find-file dunefile)))
      (with-current-buffer
          other-buffer
        (goto-char (point-max))
        (search-backward "*")
        (forward-line 2)
        (let ((rbeg (point)))
          (forward-line 2)
          (insert-into-buffer current-buffer rbeg (point))))))
  ;; in current buffer 
  (goto-char pos)
  (push-mark)
  (forward-line 2)
  (setq mark-active t)
  (orgtbl-create-or-convert-from-region
   
(defun find-executables-in-dune-file (dunefile)
  "Find all executable definitions in a dune file"
  (require 'dash)
  (save-current-buffer
    (let ((buf (find-file dunefile))
          (executables)
          )
      (with-current-buffer buf
        (beginning-of-buffer)
        (while 
            (search-forward "executable" nil t)
          (search-backward "(")
          (let* ((sexp (sexp-at-point)) 
                 (names
                  (seq-find
                   (lambda (e)
                     (and (consp e)
                          (or (equal (car e) 'name)
                              (equal (car e) 'names)))) sexp))
                 )
            (when names
              (setq executables (append executables (cdr names)))))
          (end-of-line)))
      (kill-buffer buf)
      executables)))

(defun find-dune-executables-in-project ()
  "docstring"
  (let* ((project-root (projectile-acquire-root))
         (files (projectile-project-files project-root)))
    (cl-loop 
     for f in files 
     when (equal (ff-basename f) "dune")
     ; expand file name before launching
     collect (find-executables-in-dune-file f) into dunefiles
     finally return (nconc dunefiles)
     )
    )
  )

(find-executables-in-dune-file "~/mina/mina/src/app/cli/src/dune")








