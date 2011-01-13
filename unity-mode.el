;;; unity-mode.el --- minor mode for Unity, CMock, and Ceedling
;; unit-testing, mocking, configuration integration.

(require 'unity-auto-config)
(require 'unity-rakefile)
(require 'unity-tests)

(defconst unity-mode-abbrev-table (make-abbrev-table))

(defconst unity-mode-keymap (make-sparse-keymap) "Keymap used in unity mode")

(define-key unity-mode-keymap (kbd "C-; r") 'unity-test)
(define-key unity-mode-keymap (kbd "C-; o") 'unity-test-only)
(define-key unity-mode-keymap (kbd "C-; a") 'unity-test-all)
(define-key unity-mode-keymap (kbd "C-; d") 'unity-test-delta)
(define-key unity-mode-keymap (kbd "C-; i") 'unity-toggle-test-ignored)
(define-key unity-mode-keymap (kbd "C-; t") 'unity-toggle-test-src-header-buffer)
(define-key unity-mode-keymap (kbd "C-; m") 'unity-toggle-triad)
(define-key unity-mode-keymap (kbd "C-; n") 'unity-new-menus)

(defvar unity-mode-map nil) ; No local map
(setq unity-mode-results-map (make-sparse-keymap))
(define-key unity-mode-results-map "\r" 'unity-goto-location)
(define-key unity-mode-results-map [mouse-2] 'unity-goto-location)

(defgroup unity-mode nil
  "Unity minor mode.")

;;; defvars...

(defvar unity-temp-buffer "*Unity-Buffer*")
(defvar unity-setup-performed-p nil)
(defvar unity-ruby-file-extension ".rb")
;;; setq...

(setq unity-buffer-heading-message
      (progn
        (let
            ((msg
              "\n                   Unity Mode\n
           Project Directory Structure\n\n"))
          (put-text-property
           0
           70
           'face
           '(foreground-color . "light blue") msg) msg)))

(setq unity-test-ok-message
      (progn
        (let ((msg "OK"))
          (put-text-property 0 2 'face '(foreground-color . "dark green") msg)
          msg)))
(setq unity-test-fail-message
      (progn
        (let ((msg "Failed"))
          (put-text-property 0 6 'face '(foreground-color . "dark green") msg)
          msg)))
(setq unity-test-fail-message-with-reason
      (progn
        (let ((msg "Failed: '%s'"))
          (put-text-property 0 6 'face '(foreground-color . "red") msg)
          msg)))

(defcustom unity-rake-command "rake"
  "The command for rake"
  :type 'string
  :group 'unity-mode)
(defcustom unity-project-root-dir "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/" "Project Root Directory"
  :type 'string
  :group 'unity-mode)
(defcustom unity-ceedling-root-dir "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/" "Ceedling Root Directory"
  :type 'string
  :group 'unity-mode)
(defcustom unity-unity-root-dir "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/vendor/unity/" "Unity Root Directory"
  :type 'string
  :group 'unity-mode)
(defcustom unity-cmock-root-dir "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/vendor/cmock/" "CMock Root Directory"
  :type 'string
  :group 'unity-mode)
(defcustom unity-plugins-dir "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/plugins/" "Plugins Directory"
  :type 'string
  :group 'unity-mode)
(defcustom unity-custom-plugins-dir "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/custom_plugins/" "Custom Plugins Directory"
  :type 'string
  :group 'unity-mode)
(defcustom unity-src-dir "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/src/" "Source Directory"
  :type 'string
  :group 'unity-mode)
(defcustom unity-test-dir "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/test/" "Test Files Directory"
  :type 'string
  :group 'unity-mode)
(defcustom unity-header-dir "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/src/" "Header Files Directory"
  :type 'string
  :group 'unity-mode)
(defcustom unity-mocks-dir "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/mocks" "Mock Files Directory"
  :type 'string
  :group 'unity-mode)
(defcustom unity-build-dir "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/build/" "Build Files Directory"
  :type 'string
  :group 'unity-mode)
(defcustom unity-test-file-prefix "Test" "Test File Prefix"
  :type 'string
  :group 'unity-mode)
(defcustom unity-mock-file-prefix "mock_" "Mock File Prefix"
  :type 'string
  :group 'unity-mode)
(defcustom unity-model-file-suffix "Model"
  "Model File Suffix (as in some[_model].c)"
  :type 'string
  :group 'unity-mode)
(defcustom unity-conductor-file-suffix "Conductor"
  (concat "Conductor File Suffix (as in some[unity-conductor-file-suffix].c)")
  :type 'string
  :group 'unity-mode)
(defcustom unity-hardware-file-suffix "Hardware"
  (concat "Hardware File Suffix (as in some[unity-hardware-file-suffix].c)")
  :type 'string
  :group 'unity-mode)
(defcustom unity-configurator-file-suffix "Configurator"
  (concat "Configurator File Suffix (as in some[unity-configurator-file-suffix].c)")
  :type 'string
  :group 'unity-mode)
(defcustom unity-src-file-extension ".c"
  "C Source File Extension"
  :type 'string
  :group 'unity-mode)
(defcustom unity-header-file-extension ".h"
  "C Header File Extension" 
  :type 'string
  :group 'unity-mode)

;;;###autoload
(define-minor-mode unity-mode
  "Enhanced C-mode for Unity, CMock, and Ceedling
Unit testing integration"
  :lighter "unity"
  :keymap  unity-mode-keymap)

;;; defuns...

(defun unity-is-test-file-p (file-name)
  "Returns true if the file is a test file"
  (interactive)
  (string-match
   (concat
    "^"
    unity-test-file-prefix
    ".*\\"
    unity-src-file-extension
    "$")
   (file-name-nondirectory file-name)))

(defun unity-is-src-file-p (file-name)
  "Returns t if filename has a C extension and is not a testfile"
  (interactive)
  (if (not (unity-is-test-file-p file-name))
      (string-match
       (concat
        "^.*\\"
        unity-src-file-extension
        "$")
       (file-name-nondirectory file-name))
    nil))

(defun unity-is-header-file-p (file-name)
  "Returns true if the file is a header file"
  (interactive)
  (string-match
   (concat
    "^.*\\"
    unity-header-file-extension
    "$")
   (file-name-nondirectory file-name)))

(defun unity-is-ruby-file-p (file-name)
  "Returns true if the file is a ruby file"
  (interactive)
  (string-match
   (concat
    "^.*\\"
    unity-ruby-file-extension
    "$")
   (file-name-nondirectory file-name)))

(defun unity-is-code-file-p (file-name)
  "Returns t if file-name corresponds to a testfile, src file or header file"
  (or (unity-is-src-file-p file-name)
      (unity-is-header-file-p file-name)
      (unity-is-test-file-p file-name)))

(defun unity-parent-directory (directory)
  "Returns the directory of which directory is a child"
  (file-name-directory (directory-file-name directory)))

(defun unity-root-directory-p (directory)
  "Returns t if directory is the root"
  (string= directory (unity-parent-directory directory)))

(defun unity-c-file-extension-if-necessary (file-name)
  "Adds .c file extension to file-name if it does not already have an (any) extension"
  (if (file-name-extension file-name)
      file-name ;; file has a extension already so do nothing
    (concat file-name unity-src-file-extension)))

(defun unity-create-src-file-name (file-name)
  "Returns file-name but converted into a non-test (source) file name"
  (concat (file-name-directory file-name)
          (unity-c-file-extension-if-necessary 
           (replace-regexp-in-string
            (concat "^" unity-test-file-prefix)
            ""
            (file-name-nondirectory file-name)))))

(defun unity-create-test-file-name (file-name)
  "Returns file-name but converted into a test file name"
  (concat (file-name-directory file-name)
          (unity-c-file-extension-if-necessary 
           (replace-regexp-in-string
            "^"
            unity-test-file-prefix
            (file-name-nondirectory file-name)))))

(defun unity-header-to-src-file-name (file-name)
  "Find the source file for a header file"
  (replace-regexp-in-string
   (concat unity-header-file-extension "$")
   unity-src-file-extension
   file-name))

(defun unity-src-to-header-file-name (file-name)
  "Find the header file for a source file"
  (replace-regexp-in-string
   (concat unity-src-file-extension "$")
   unity-header-file-extension
   file-name))

(defun unity-buffer-is-test-p ()
  "Returns true if the current buffer is a test file"
  (and (buffer-name)
       (unity-is-test-file-p (buffer-name))))

(defun unity-buffer-is-header-p ()
  "Returns true if the current buffer is a header file"
  (and (buffer-name)
       (unity-is-header-file-p (buffer-name))))

(defun unity-search-for-test-directory-has-src-p (file-name)
  "Returns t if /src is found in appropriate place" 
  (file-directory-p (concat (unity-search-for-test-directory file-name) "/src")))

(defun unity-source-directory-has-src? (file-name)
  (file-directory-p (concat (unity-source-directory file-name) "/src")))

(defun unity-beginning-of-test()
  "Moves point to the beginning of the test in which the point currently is."
  (interactive)
  (let ((start (point)))
    (goto-char 
     (save-excursion
       (end-of-line)
       (unless (and (search-backward-regexp "^[ \t]*void[ \t]+[Tt]est.*([ \t]*void[ \t]*)[ \t]*" nil t)
                    (save-excursion (c-end-of-defun) (< start (point))))
         (error "Unable to find a test"))
       (point)))))

(defun unity-test-ignored-p ()
  "True if the test under point is ignored. Otherwise false"
  (interactive)
  (save-excursion
    (unity-beginning-of-test)
    (re-search-forward "^[ \t\n]*TEST_IGNORE[ \t]*([ \t]*)" (save-excursion (c-end-of-defun) (point)) t)))

(defun unity-toggle-test-ignored ()
  "Disables active tests and enables ignored tests."
  (interactive)
  (if (unity-test-ignored-p)
      (unity-enable-test)
    (unity-disable-test)))

(defun unity-disable-test ()
  "Disable the test in which the point is located"
  (interactive)
  (when (not (unity-test-ignored-p))   
    (save-excursion
      (unity-beginning-of-test)
      (search-forward-regexp "^[ \t]*void[ \t]+[Tt]est.*([ \t]*void[ \t]*)[ \t\n]*{" (save-excursion (c-end-of-defun) (point)))
      (insert "\nTEST_IGNORE();")
      (indent-for-tab-command))))

(defun unity-enable-test ()
  "Enable the test in which the point is located"
  (interactive)
  (when (unity-test-ignored-p)
    (save-excursion
      (unity-beginning-of-test)
      (search-forward-regexp "^[ \t]*TEST_IGNORE[ \t]*([ \t]*)" (save-excursion (c-end-of-defun) (point)))
      (beginning-of-line)
      (delete-region (save-excursion (beginning-of-line) (point)) 
                     (save-excursion (forward-line 1) (point))))))

;; (defun unity-verify ()
;;   "Runs the specified test, or the test file for the current buffer."
;;   (interactive)
;;   (unity-run-single-file (unity-find-test-for-src-file (buffer-file-name)) (unity-core-options ())))

(defun unity-search-for-test-directory (file-or-directory)
  "Returns the nearest test directory that could contain tests for file-or-directory. The function is passed either a file or directory, and is recursive."
  (if (file-directory-p file-or-directory)
      (or
       (first (directory-files file-or-directory t "^test$"))
       (if (unity-root-directory-p file-or-directory)
           nil
         (unity-search-for-test-directory (unity-parent-directory file-or-directory))))
    (unity-search-for-test-directory (unity-parent-directory file-or-directory))))

(defun unity-search-for-project-root-by-rakefile (&optional directory)
  "Attempts to find the root directory of the project by walking up the directory tree until it finds a rake file"
  (let ((directory (file-name-as-directory (or directory default-directory))))
    (cond ((unity-root-directory-p directory) nil)
          ((file-exists-p (concat directory "rakefile.rb")) directory)
          (t (unity-search-for-project-root-by-rakefile (file-name-directory (directory-file-name directory)))))))

(defun unity-test-all ()
  (interactive)
  (message "Testing ALL")
  (let ((default-directory "/home/martyn/ceedling5/trunk/examples/temp_sensor/"))
    ;;(cd "/home/martyn/ceedling5/trunk/examples/temp_sensor/")
    (shell-command (concat "rake test:delta  " "-f /home/martyn/ceedling5/trunk/examples/temp_sensor/rakefile.rb" ))
    (end-of-buffer-other-window 0)))

(defvar unity-backtrace-key-map
  "The keymap which is bound to marked trace frames.")

(defvar unity-font-lock-keywords
  (list
   '("^.*" 0
     `(face font-lock-warning-face
            message ((file-name . ,(buffer-substring-no-properties (match-beginning 2) (match-end 2)))
                     (line-number . ,(string-to-number (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
            follow-link t
            mouse-face highlight
            help-echo "RET to visit location"
            keymap unity-backtrace-key-map))
   ))
;; (defvar unity-font-lock-keywords
;;   (list
;;    '("^[[:space:]]*\\[?\\(\\([[:graph:]]*\\):\\([[:digit:]]+\\)\\):" 1 ; test/unit backtrace
;;      `(face font-lock-warning-face
;; 	    message ((file-name . ,(buffer-substring-no-properties (match-beginning 2) (match-end 2)))
;; 		     (line-number . ,(string-to-number (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
;; 	    follow-link t
;; 	    mouse-face highlight
;; 	    help-echo "RET to visit location"
;; 	    keymap unity-backtrace-key-map))
;;    '("^[[:alnum:]_]+(.+) \\[\\(\\([[:graph:]]*\\):\\([[:digit:]]+\\)\\)\\]:" 1 ; rspec backtrace
;;      `(face font-lock-warning-face
;; 	    message ((file-name . ,(buffer-substring-no-properties (match-beginning 2) (match-end 2)))
;; 		     (line-number . ,(string-to-number (buffer-substring-no-properties (match-beginning 3) (match-end 3)))))
;; 	    follow-link t
;; 	    mouse-face highlight
;; 	    help-echo "RET to visit location"
;; 	    keymap unity-backtrace-key-map))))

(defun unity-test-mode ()
  "minor mode for Unity, CMock, and Ceedling
 unit-testing, mocking, configuration integration"
  (interactive)
  (kill-all-local-variables)
  (use-local-map unity-mode-map)
  (make-local-variable 'view-read-only)
  (set (make-local-variable 'font-lock-defaults)
       '((unity-font-lock-keywords) nil nil))
  ;;(setq major-mode 'unity-mode)
  ;;(run-hooks 'unity-test-mode-hook))
  )

(defun unity-run-file ()
  (interactive)
  (setq unity-buffer (get-buffer-create unity-temp-buffer))
  ;;  (let ((test-file (find-unity-file)))
  (let ((test-file "/home/martyn/ceedling5/trunk/examples/temp_sensor/test/TestAdcConductor.c"))
    (if t ;; test-file
        (unity-run-test-file test-file unity-buffer)
      (message unity-not-found-message))))

(defun invoke-test-file (file buffer)
  (message "Running %s " file)
  (display-buffer buffer)
  (setq unity-last-run file)
  (save-excursion
    (set-buffer buffer)
    (setq buffer-read-only t)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (set-auto-mode-0 'unity-mode nil)
      ;;      (let ((args (append (list command-string) options)))
      (let ((args ()))
        (let ((directory unity-project-root-dir)
              (previous-directory default-directory))
          (and directory (cd directory))
          
          (let ((proc ((compile "rake test:delta")
                       ;;              apply 'start-process "unity-test" buffer
                       ;;              "rake" '("test:delta"))
                       ))))
          (set-process-sentinel proc 'unity-runner-sentinel))
        (and directory (cd previous-directory))))))

(defun unity-runner-sentinel (process event)
  (save-excursion
    (set-buffer unity-buffer)
    (cond
     ((string= "zzzfinished\n" event) (message unity-test-ok-message))
     ((string= "exited abnormally with code 1\n" event) (message unity-test-fail-message))
     (t (progn
          (string-match "\\(.*\\)[^\n]" event)
          (message unity-test-fail-message-with-reason (match-string 1 event)))))))

;; TODO what is the sensible way to do this?..
(defun unity-colour (msg colour)
  (progn
    (let ((msg msg))
      (put-text-property
       0 (length msg) 'face
       (or (if (equal colour "green")
               '(foreground-color . "green"))
           (if (equal colour "dark green")
               '(foreground-color . "dark green"))
           (if (equal colour "red")
               '(foreground-color . "red"))
           (if (equal colour "yellow")
               '(foreground-color . "yellow"))
           (if (equal colour "light blue")
               '(foreground-color . "light blue")))
       msg)
      msg)))

(provide 'unity-mode)

(defun unity-find-root-dir (&optional reference-file)
  (if (unity-is-code-file-p reference-file)
      (unity-search-for-project-root-by-rakefile
       (file-name-directory reference-file))))

(defun unity-check-for-ceedling-directories-p  (directory)
  "Search directory for relevant ceedling directories"
  (and
   (file-directory-p (concat directory "config"))
   (file-directory-p (concat directory "lib"))
   (file-directory-p (concat directory "plugins"))
   (file-directory-p (concat directory "vendor"))))

(defun unity-search-for-ceedling-root-by-presence-of-relevant-dirs (&optional directory)
  "Attempts to find the Ceedling root directory of the project by walking up the directory tree until it finds Ceedling-relevant directories"
  (let ((directory (file-name-as-directory (or directory default-directory))))
    (cond ((unity-root-directory-p directory) nil)
          ((unity-check-for-ceedling-directories-p directory) directory)
          (t (unity-search-for-ceedling-root-by-presence-of-relevant-dirs
              (file-name-directory (directory-file-name directory)))))))

(defun unity-check-for-unity-root-relative-to-ceedling-p (directory)
  (file-directory-p (concat directory "vendor/unity/")))

(defun unity-check-for-custom-plugins-relative-to-ceedling-p (directory)
  (file-directory-p (concat directory "custom_plugins/")))

(defun unity-check-for-unity-root-relative-to-ceedling-p (directory)
  (file-directory-p (concat directory "vendor/unity/")))

(defun unity-check-for-cmock-root-relative-to-ceedling-p (directory)
  (file-directory-p (concat directory "vendor/cmock/")))

(defun unity-rakefile-set-target (rakefile target)
  "Takes template project rakefile and sets correct ceedling rakefile target which will typically be \"ceedling-root-dir/lib/rakefile.rb\".

Return a new string containing the rakefile contents with ceedling-rakefile-target.rb replaced with TARGET."

  (replace-regexp-in-string
   "ceedling-rakefile-target.rb"
   target
   rakefile))

(defun unity-rakefile-backup (old-rakefile &optional test-time)
  (let ((temp (unity-generate-backup-name old-rakefile test-time)))
    (if (not temp)
        (error (concat old-rakefile  " not found!"))
      (if (not test-time) (rename-file old-rakefile temp))
      temp)))


(defun unity-generate-backup-name (name &optional test-time)
  (if (unity-is-ruby-file-p name)
      (replace-regexp-in-string
       " " "_" (concat
                (replace-regexp-in-string ".rb" "" name) "_"
                (current-time-string test-time) ".rb"))))

(defun unity-switch-src-header-buffer (file-name)
  (if (unity-test-file-exists-p (unity-src-to-header-file-name (buffer-file-name)
  ))))

(defun unity-test-file-exists-p (test-file-name)
  "Returns t if passed a valid path to a C source file"
  (string-match
   (concat "^" (expand-file-name
                (regexp-quote
                 (concat
                  unity-project-root-dir test-file-name "/src"))))
   test-file-name))

(defun unity-target-src-file-p (file-name)
  "Returns"
  (string-match
   (concat "^" (expand-file-name
                (regexp-quote
                 (concat
                  (unity-search-for-project-root-by-rakefile file-name) "/src"))))
   file-name))

(defun unity-file-exists-p (file-name file-type)
  (file-exists-p
   (concat
    (unity-select-path
     file-type)
    file-name)))

(defun unity-switch-file-name (file-name switch-type)
  (cond ((string= switch-type "test-to-src")
         (unity-create-src-file-name file-name))
        ((string= switch-type "src-to-header")
         (unity-src-to-header-file-name file-name))
        ((string= switch-type "test-to-header")
         (unity-src-to-header-file-name
          (unity-create-src-file-name file-name)))
        ((string= switch-type "src-to-test")
         (unity-create-test-file-name file-name))
        ((string= switch-type "header-to-test")
         (unity-create-test-file-name
          (unity-header-to-src-file-name file-name)))
        ((string= switch-type "header-to-src")
         (unity-header-to-src-file-name file-name))
        (t (error "ERROR! Invalid switch-type in unity-switch-file-name!"))))

(defun unity-dest-file-type (switch-type)
   (concat (replace-regexp-in-string "^.*-" "" switch-type) "-type"))

(defun unity-select-path (file-type)
  (cond ((equal file-type "src-type")
         unity-src-dir)
        ((equal file-type "test-type")
         unity-test-dir)
        ((equal file-type "header-type")
         unity-header-dir)
        (t (error "ERROR! Invalid file-type in unity-select-path"))))

(defun unity-switch-buffer (file-name switch-type &optional test)
  (let ((temp-name
         (unity-switch-file-name
          file-name
          switch-type)))
    (if (unity-file-exists-p
         temp-name
         (unity-dest-file-type switch-type))
        (if (not test)
            (find-file
             (concat
              (unity-select-path (unity-dest-file-type switch-type))
              temp-name))
          temp-name))))
  
(defun unity-toggle-test-src-header-buffer ()
  "Toggle between test source file "
  (interactive)
  (let ((file-name (file-name-nondirectory buffer-file-name)))
    (cond ((unity-is-test-file-p file-name)
           (if (not (unity-switch-buffer file-name "test-to-src"))
               (if (not (unity-switch-buffer file-name "test-to-header"))
                   (error "No matching source or header file!"))))
          
          ((unity-is-src-file-p file-name)
           (if (not (unity-switch-buffer file-name "src-to-header"))
               (if (not (unity-switch-buffer file-name "src-to-test"))
                   (error "No matching test or header file!"))))
          
          ((unity-is-header-file-p file-name)
           (if (not (unity-switch-buffer file-name "header-to-test"))
               (if (not (unity-switch-buffer file-name "header-to-src"))
                   (error "No matching src or test file!"))))
          ( t (error "File is neither test source or header file!")))))

