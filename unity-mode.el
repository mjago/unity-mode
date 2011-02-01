;;; unity-mode.el --- minor mode for Unity, CMock, and Ceedling
;; unit-testing, mocking, configuration integration.

(require 'unity-custom-settings)
(require 'unity-auto-config)
(require 'unity-rakefile)

;; Uncomment to allow self-test with ert...
(require 'unity-tests)

(defconst unity-enable-tests t)
(defconst unity-mode-abbrev-table (make-abbrev-table))

(defconst unity-mode-keymap
 
  (make-sparse-keymap) "Keymap used in unity mode")

(define-key unity-mode-keymap
  (kbd "C-; C-i") 'unity-toggle-test-ignored)
(define-key unity-mode-keymap
  (kbd "C-; C-t") 'unity-cycle-test-src-header-buffer)
(define-key unity-mode-keymap
  (kbd "C-; C-m") 'unity-cycle-MCH-buffer)
(define-key unity-mode-keymap
  (kbd "C-; C-n") 'unity-cycle-alpha-ascending)
(define-key unity-mode-keymap
  (kbd "C-; C-p") 'unity-cycle-alpha-descending)

(define-key unity-mode-keymap
  (kbd "C-; r") 'unity-test)
(define-key unity-mode-keymap
  (kbd "C-; o") 'unity-test-only)
(define-key unity-mode-keymap
  (kbd "C-; a") 'unity-test-all)
(define-key unity-mode-keymap
  (kbd "C-; d") 'unity-test-delta)
 
(setq unity-temp-sensor-project-list
      ;; order group position pattern              paths         
      '((0             '("Conductor" "_conductor") unity-src-dir)
        (1             '("Model" "_model")         unity-src-dir)
        (2             '("Hardware" "_hardware")   unity-src-dir)
        (3             '("Other" "_other")         unity-src-dir)
        (4             '("Interrupt" "_interrupt") unity-src-dir)))
;;  name    included    order   prefixes        suffixes   extensions    locations
;;          in 
;;          primative
;;          switch?

(setq unity-temp-sensor-primatives-list
'(('test    t           0       '("Test test_") '("")      '("c" "C")    '("Test/" "test/")        )
  ('c-src   t           1       '("")           '("")      '("c" "C")    '("lib/" "src/")          )
  ('asm-src t           2       '("")           '("")      '("s" "asm")  '("asm/" "src/")          )
  ('c-inc   t           3       '("")           '("")      '("h" "H")    '("inc/" "src/" "lib/")   )
  ('org     f           4       '("")           '("")      '("org")      '(c-test-dir "./" "docs/"))
  ('yml     f           5       '("")           '("")      '("yml")      '(c-test-dir)             )))

;; setq unity-temp-sensor-project-list
;;       ;; order group position pattern                     path         
;;       '((0     2     3        '("Conductor" "_conductor") unity-src-dir)
;;         (1     2     3        '("Model" "_model")         unity-src-dir)
;;         (2     2     3        '("Hardware" "_hardware")   unity-src-dir)
;;         (3     2     3        '("Other" "_other")         unity-src-dir)
;;         (4     2     3        '("Interrupt" "_interrupt") unity-src-dir)))

(setq unity-dev-project-list
      ;; order patterns                    path
      '((0     '("Conductor" "_conductor") unity-src-dir)
        (1     '("Model" "_model")         unity-src-dir)
        (2     '("Hardware" "_hardware")   unity-src-dir)
        (3     '("Other" "_other")         unity-src-dir)
        (4     '("Interrupt" "_interrupt") unity-src-dir)))

(setq unity-suffix-4 
      '((pattern-0  '("Configurator" "_configurator"))
        (pattern-1  '("Sensor" "_sensor"))
        (pattern-2  '("Handler" "_handler"))
        (pattern-3  '("Wrapper" "_wrapper"))
        (pattern-F  '())))

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
          (put-text-property
           0 (length "OK") 'face
           '(foreground-color . "dark green") msg)
          msg)))
(setq unity-test-fail-message
      (progn
        (let ((msg "Failed"))
          (put-text-property
           0 (length "Failed") 'face
           '(foreground-color . "dark green") msg)
          msg)))
(setq unity-test-fail-message-with-reason
      (progn
        (let ((msg "Failed: '%s'"))
          (put-text-property
           0 6 'face '(foreground-color . "red") msg)
          msg)))

;;;###autoload
(define-minor-mode unity-mode
  "Enhanced C-mode for Unity, CMock, and Ceedling
Unit testing integration"
  :lighter "unity"
  :keymap  unity-mode-keymap)

;;; defuns...

(defun dbg(msg)
  (if (not msg)
      (setq msg "nil"))
  (if (equal msg t)
      (setq msg "t"))
  (error (concat "dbg: " msg))) 

(add-hook 'after-save-hook 'unity-eval-src-and-tests)

(defun unity-switch-src-control-file()
  "Fast route to unity-mode.org and back"
  (interactive)
  (let ((project-file 
         "/home/martyn/Dropbox/OrgData/unity-mode.org"))
    (if (not (equal (buffer-file-name) project-file))
        (progn (setq unity-last-buffer (buffer-file-name))
               (message (concat "switching to " project-file))
               (find-file project-file))
      (progn (message (concat "switching to " unity-last-buffer))
             (switch-to-buffer  (file-name-nondirectory unity-last-buffer))))))

(defun unity-replace-regex-in-string (regex replaced string)
  "case sensitive replace-regexp-in-string replacement"
  (let ((case-fold-search nil)
        (case-replace nil))
    (replace-regexp-in-string
     regex replaced string t t nil nil)))

(defun unity-file-prefix (file-type)
  "case sensitive replace-regexp-in-string replacement"
  (cond ((equal file-type "test-file")
         unity-test-file-prefix)
        ((or (equal file-type "src-file")
             (equal file-type "header-file")
             (equal file-type "conductor-file")
             (equal file-type "model-file")
             (equal file-type "hardware-file"))
         "")
        ( t (unity-error-with-param
             "Invalid file-type"
             file-type
             "in unity-file-prefix"))))

(defun unity-file-suffix (file-type)
  "Returns suffix matching FILE-TYPE such as test-file"
  (cond ((or (equal file-type "test-file")
             (equal file-type "src-file")
             (equal file-type "header-file"))
         "")
        ((equal file-type "conductor-file")
         unity-conductor-file-suffix)
        ((equal file-type "model-file")
         unity-model-file-suffix)
        ((equal file-type "hardware-file")
         unity-model-file-suffix)
        ( t (unity-error-with-param 
             "Invalid file-type"
             file-type
             "in unity-file-suffix"))))

(defun unity-error (string &optional test)
  (let ((error-msg
         (concat
          "Error! "
          (if (not (equal string nil))
              (concat
               string "!")
            "(nil error message) !"))))
    (if (not test)
        (error error-msg)
      error-msg)))

(defun unity-error-with-param (string1 parameter string2 &optional test)
  (let ((parameter
         (if (not (equal parameter nil))
             parameter
           "nil")))
    (let ((error-msg
           (concat
            "Error! " string1 " (" parameter ") " string2 "!")))
      (if (not test)
          (error error-msg)
        error-msg))))

(defun unity-file-prefix-list ()
  `(, unity-test-file-prefix))

(defun unity-file-suffix-list ()
  `(,unity-model-file-suffix
    ,unity-conductor-file-suffix
    ,unity-hardware-file-suffix))
 
(defun unity-string-exact-match (regex string &optional start)
  "case sensative version of string-match"
  (let ((case-fold-search nil)
        (case-replace nil))
    (if(and (equal regex "")
            (not(equal string "")))
        nil
      (if (equal 0 (string-match regex string start))
          t
        nil))))

(defun unity-check-suffix-p (file-name file-type)
  (let ((case-fold-search nil)
        (case-replace nil))
    (string-match
     (concat
      (unity-file-suffix file-type)
      "$")
     (file-name-sans-extension file-name))))

(defun unity-check-extension-p (file-name file-type)
  "Returns true if the file name extension is correct"
  (equal
   (unity-read-extension file-name)
   (cond ((equal file-type "header-file")
          unity-header-file-extension)
         ((or (equal file-type "src-file")
              (equal file-type "test-file") 
              (equal file-type "conductor-file")
              (equal file-type "hardware-file")
              (equal file-type "model-file"))
          unity-src-file-extension)
         ( t (unity-error-with-param
              "Invalid file-type"
              file-type
              "in unity-check-extension-p")))))

(defun unity-is-file-type-p (file-name file-type)
  "Returns true if the file name matches the file type"
  (and (unity-check-prefix-p file-name file-type)
       (unity-check-suffix-p file-name file-type)
       (unity-check-extension-p file-name file-type)))

(defun unity-is-test-file-p (file-name)
  "Returns true if the file is a test file"
  (if (equal (unity-read-extension file-name)
             unity-src-file-extension)
      (unity-check-prefix-p file-name "test-file")
    nil))

(defun unity-is-src-file-p (file-name)
  "Returns t if filename has a C extension and is not a testfile"
  (and (unity-check-prefix-p file-name "src-file")
       (unity-check-extension-p file-name "src-file")))

(defun unity-is-header-file-p (file-name)
  "Returns true if the file is a header file"
  (unity-is-file-type-p file-name "header-file"))

(defun unity-check-prefix-p (file-name file-type)
  (let ((prefix
         (unity-file-prefix file-type)))
    
    (unity-string-exact-match
     (concat
      "^"
      prefix
      ".*\\.*")
     (file-name-nondirectory file-name))))

(defun unity-is-pattern-file-p (file-name pattern-match)
  "Returns true if FILE-NAME is a file of FILE-TYPE."
  (unity-string-exact-match
   (concat
    "^.*"
    pattern-match
    "\\" 
    unity-src-file-extension
    "$")
   (file-name-nondirectory file-name)))

(defun unity-is-ruby-file-p (file-name)
  "Returns true if the file is a ruby file"
  (unity-string-exact-match
   (concat
    "^.*\\"
    unity-ruby-file-extension
    "$")
   (file-name-nondirectory file-name)))

(defun unity-is-code-file-p (file-name)
  "Returns t if file-name corresponds to a testfile,
source file or header file"
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
  "Adds .c file extension to file-name if it does not
already have an (any) extension"
  (if (file-name-extension file-name)
      file-name ;; file has a extension already so do nothing
    (concat file-name unity-src-file-extension)))

(defun unity-create-src-file-name (file-name)
  "Returns file-name but converted into a non-test (source) file name"
  (concat (file-name-directory file-name)
          (unity-c-file-extension-if-necessary 
           (unity-replace-regex-in-string
            (concat "^" unity-test-file-prefix)
            ""
            (file-name-nondirectory file-name)))))

(defun unity-create-test-file-name (file-name)
  "Returns file-name but converted into a test file name"
  (concat (file-name-directory file-name)
          (unity-c-file-extension-if-necessary 
           (unity-replace-regex-in-string
            "^"
            unity-test-file-prefix
            (file-name-nondirectory file-name)))))

(defun unity-header-to-src-file-name (file-name)
  "Find the source file for a header file"
  (unity-replace-regex-in-string
   (concat "\\" unity-header-file-extension "$")
   unity-src-file-extension
   file-name))

(defun unity-src-to-header-file-name (file-name)
  "Find the header file for a source file"
  (unity-replace-regex-in-string
   (concat "\\" unity-src-file-extension "$")
   unity-header-file-extension
   file-name))

(defun unity-conductor-to-hardware-file-name (file-name)
  "Return Hardware src name matching Conductor src or header name

If argument FILE-NAME is not a Conductor src or header name,
return original file-name"
  
  (unity-replace-regex-in-string
   (concat unity-conductor-file-suffix "\\"
           unity-src-file-extension "$")
   (concat unity-hardware-file-suffix
           unity-src-file-extension)
   (unity-header-to-src-file-name file-name)))

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
  (file-directory-p (concat (unity-search-for-test-directory
                             file-name) "/src")))

(defun unity-source-directory-has-src? (file-name)
  (file-directory-p (concat (unity-source-directory file-name) "/src")))

(defun unity-beginning-of-test()
  "Moves point to the beginning of the test in which
the point currently is."
  (interactive)
  (let ((start (point)))
    (goto-char 
     (save-excursion
       (end-of-line)
       (unless
           (and
            (search-backward-regexp
             "^[ \t]*void[ \t]+[Tt]est.*([ \t]*void[ \t]*)[ \t]*" nil t)
            (save-excursion (c-end-of-defun) (< start (point))))
         (unity-error "Unable to find a test"))
       (point)))))

(defun unity-test-ignored-p ()
  "True if the test under point is ignored. Otherwise false"
  (interactive)
  (save-excursion
    (unity-beginning-of-test)
    (re-search-forward
     "^[ \t\n]*TEST_IGNORE[ \t]*([ \t]*)"
     (save-excursion
       (c-end-of-defun) (point)) t)))

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
      (search-forward-regexp
       "^[ \t]*void[ \t]+[Tt]est.*([ \t]*void[ \t]*)[ \t\n]*{"
       (save-excursion
         (c-end-of-defun)
         (point)))
      (insert "\nTEST_IGNORE();")
      (indent-for-tab-command))))

(defun unity-enable-test ()
  "Enable the test in which the point is located"
  (interactive)
  (when (unity-test-ignored-p)
    (save-excursion
      (unity-beginning-of-test)
      (search-forward-regexp
       "^[ \t]*TEST_IGNORE[ \t]*([ \t]*)"
       (save-excursion
         (c-end-of-defun)
         (point)))
      (beginning-of-line)
      (delete-region (save-excursion (beginning-of-line) (point)) 
                     (save-excursion (forward-line 1) (point))))))

;; (defun unity-verify ()
;;   "Runs the specified test, or the test file for the current buffer."
;;   (interactive)
;;   (unity-run-single-file (unity-find-test-for-src-file (buffer-file-name)) (unity-core-options ())))

(defun unity-search-for-test-directory (file-or-directory)
  "Returns the nearest test directory that could contain tests
for file-or-directory. The function may be passed a file or
directory, and is recursive."
  (if (file-directory-p file-or-directory)
      (or
       (first (directory-files file-or-directory t "^test$"))
       (if (unity-root-directory-p file-or-directory)
           nil
         (unity-search-for-test-directory
          (unity-parent-directory file-or-directory))))
    (unity-search-for-test-directory
     (unity-parent-directory file-or-directory))))

(defun unity-search-for-project-root-by-rakefile (&optional directory)
  "Attempts to find the root directory of the project by
walking up the directory tree until it finds a rake file"
  (let ((directory (file-name-as-directory (or directory default-directory))))
    (cond ((unity-root-directory-p directory) nil)
          ((file-exists-p (concat directory "rakefile.rb")) directory)
          (t (unity-search-for-project-root-by-rakefile
              (file-name-directory
               (directory-file-name directory)))))))

(defun unity-test-all ()
  (interactive)
  (message "Testing ALL")
  (let ((default-directory
          "/home/martyn/ceedling5/trunk/examples/temp_sensor/"))
    ;;(cd "/home/martyn/ceedling5/trunk/examples/temp_sensor/")
    (shell-command
     (concat "rake test:delta  "
             "-f /home/martyn/ceedling5/trunk/\
examples/temp_sensor/rakefile.rb" ))
    (end-of-buffer-other-window 0)))

(defvar unity-backtrace-key-map
  "The keymap which is bound to marked trace frames.")

(defvar unity-font-lock-keywords
  (list
   '("^.*" 0
     `(face font-lock-warning-face
            message
            ((file-name
              . ,(buffer-substring-no-properties
                  (match-beginning 2) (match-end 2)))
             (line-number
              . ,(string-to-number
                  (buffer-substring-no-properties
                   (match-beginning 3) (match-end 3)))))
            follow-link t
            mouse-face highlight
            help-echo "RET to visit location"
            keymap unity-backtrace-key-map))
   ))

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
  (let((test-file
        "/home/martyn/ceedling5/trunk/examples/temp_sensor/test/TestAdcConductor.c"))
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
     ((string= "finished\n" event) (message unity-test-ok-message))
     ((string= "exited abnormally with code 1\n" event)
      (message unity-test-fail-message))
     (t (progn
          (string-match "\\(.*\\)[^\n]" event)
          (message
           unity-test-fail-message-with-reason
           (match-string 1 event)))))))

(defun unity-colour (msg colour)
  (put-text-property
   0 (length msg) 'face `(foreground-color .,colour) msg) msg)

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

(defun unity-search-for-ceedling-root-by-presence-of-relevant-dirs
  (&optional directory)
  "Attempts to find the Ceedling root directory of the
project by walking up the directory tree until it finds
Ceedling-relevant directories"
  (let ((directory (file-name-as-directory
                    (or directory default-directory))))
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
  "Takes template project rakefile and sets correct ceedling
rakefile target which will typically be
\"ceedling-root-dir/lib/rakefile.rb\".

Return a new string containing the rakefile contents with ceedling-rakefile-target.rb replaced with TARGET."

  (unity-replace-regex-in-string
   "ceedling-rakefile-target.rb"
   target
   rakefile))

(defun unity-rakefile-backup (old-rakefile &optional test-time)
  (let ((temp (unity-generate-backup-name old-rakefile test-time)))
    (if (not temp)
        (unity-error (concat old-rakefile  " not found"))
      (if (not test-time) (rename-file old-rakefile temp))
      temp)))

(defun unity-generate-backup-name (name &optional test-time)
  (if (unity-is-ruby-file-p name)
      (unity-replace-regex-in-string
       " " "_" (concat
                (unity-replace-regex-in-string ".rb" "" name) "_"
                (current-time-string test-time) ".rb"))))

(defun unity-file-exists-p (file-name file-type)
  "Determines whether the argument FILE-NAME is present in the
unity directories.

Second argument FILE-TYPE is a string containing file type
such as \"str-type\"."

  (file-exists-p
   (concat
    (unity-select-path
     file-name
     file-type)
    file-name)))

(defun unity-dest-file-type (switch-type)
  "Returns SWITCH-TYPE as in \"src-to-header \" with \" header-type\""
  (concat
   (unity-replace-regex-in-string "^.*-" "" switch-type)
   "-type"))

(defun unity-select-path (file-name file-type)
  (cond ((equal file-type "src-type")
         unity-src-dir)
        ((equal file-type "test-type")
         unity-test-dir)
        ((equal file-type "header-type")
         unity-header-dir)
        (t (unity-error-with-param
            "Invalid file-type"
            file-type
            "in unity-select-path"))))
  
(defun unity-switch-buffer (file-name switch-type &optional test)
  (let ((temp-name
         (unity-switch-file-name
          file-name switch-type)))
    (if (unity-file-exists-p
         temp-name
         (unity-dest-file-type switch-type))
        (when (not test)
          (find-file
           (concat
            (unity-select-path
             file-name
             (unity-dest-file-type switch-type))
            temp-name)))
      (setq temp-name nil))
    temp-name))

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
        (t (unity-error-with-param
            "Invalid switch-type"
            switch-type
            "in unity-switch-file-name!"))))

(defun unity-buffer-name-nondirectory-or-test-file (test-name)
  (file-name-nondirectory
   (if test-name
       test-name
     (buffer-file-name))))
      
  ;; (if(not (unity-is-test-file-p test-name))
  ;;   file-name))
 
(defun unity-string-exact-match (regex string &optional start)
  (let ((case-fold-search nil)
        (case-replace nil))
    (if(and (equal regex "")
            (not(equal string "")))
        nil
      (if (equal 0 (string-match regex string start))
          t
        nil))))

(defun unity-read-extension (file-name)
  (let ((ext nil))
    (setq ext (file-name-extension file-name))
    (cond ((> (length ext) 0)
           (setq ext (concat "." ext)))
          ((not ext)
           (setq ext ""))
          ((equal ext "")
           (setq ext ""))
          (t (unity-errorwith-param
              "Invalid extension "
              ext
              "in unity-read-extension")))
    ext))

(defun unity-read-attribute (file-name attrib-type)
  "Reads file-name attribute and leaves in file-name.

Parameters:
FILE-NAME file-name (sring)
ATTRIB-TYPE attribute type (string)
"
  (cond ((equal attrib-type "extension-type")
         (unity-read-extension file-name))
        ( t (unity-error-with-param
             "Invalid attrib-type"
             attrib-type
             "in unity-read-attribute"))))

(defun unity-remove-extension (file-name) 
  (let ((temp nil)
        (ext nil))
    (setq ext
          (unity-read-attribute file-name "extension-type"))
    (setq temp ;;(concat file-name "test"))
          (unity-replace-regex-in-string
           ext
           ""
           file-name))
    temp))

(defun operate-sub-extension (callee file-name &optional optional)
  (let ((ext (unity-read-extension file-name)))
    (concat
     (funcall callee (unity-remove-extension file-name) optional)
     ext)))

(defun operate-sub-prefix (callee file-name &optional optional)
  (let ((ext (unity-read-extension file-name)))
    (concat
     (funcall callee (unity-remove-extension file-name) optional)
     ext)))

(defun unity-read-prefix (file-name)
  "return prefix if it exists or nil"
  (let ((case-fold-search nil)
        (case-replace nil)
        (prefix "")
        (temp nil))
    (mapcar (lambda (x)
              (setq temp
                    (string-match
                     (concat "^" x)
                     file-name))
              (if temp
                  (setq prefix x)))
            (unity-file-prefix-list))
    prefix))

(defun unity-read-suffix  (file-name)
  (let ((case-fold-search nil)
        (case-replace nil)
        (suffix "")
        (temp nil))
    (mapcar (lambda (x)
              (setq temp
                    (string-match
                     (concat  x "$")
                     (file-name-sans-extension file-name)))
              (if temp
                  (setq suffix x)))
            (unity-file-suffix-list))
    suffix))

(defun unity-index-current-buffer(file-name &optional test)
 (if(or
      test
      (buffer-file-name))
     (let ((files
             (directory-files 
              (file-name-directory
               file-name)))
            (j 0)
            (searching t)
            (result nil))
        (while searching
          (setq j (+ 1 j))
          (setq result (nth j files))
          (if(or(equal result (file-name-nondirectory file-name))
                (equal nil result))
              (setq searching nil)))
        (if(not result)
            (setq j nil))
        j)
    (unity-error "File must exist on disk - please save first")))

(defun unity-switch-direction-list (file-name)
  (cond ((unity-is-test-file-p file-name)
         (list "test-to-src" "test-to-header"))
        ((unity-is-src-file-p file-name)
         (list "src-to-header" "src-to-test"))
        ((unity-is-header-file-p file-name)
         (list "header-to-test" "header-to-src"))
        (t ((error
             (concat
              "No relevant cycle found for"
              file-name))))))

(defun unity-find-and-switch-buffer (file-name &optional test)
  (let ((temp t)
        (temp-name nil)
        (switch-direction-list
         (unity-switch-direction-list file-name)))

    (mapcar (lambda (x)
              (setq temp (length switch-direction-list))
              (while (and
                      (> temp 0)
                      (not temp-name))
                (setq temp-name
                      (unity-switch-buffer
                       file-name x test))
                (setq temp (- temp 1))))
            switch-direction-list)
    
    (if(not temp-name)
        (error "No matching pattern files found!"))
    temp-name))

(defun unity-cycle-test-src-header-buffer (&optional test-file)
  "Toggle between test source file "
  (interactive)
;;  (error (unity-buffer-name-nondirectory-or-test-file test-file))
  (unity-find-and-switch-buffer
   (unity-buffer-name-nondirectory-or-test-file
    
    test-file)test-file))
 
(defun unity-cycle-MCH-buffer (&optional test-file)
  "Cycle between model conductor and hardware buffers "
  (interactive)
  (let ((file-name
         (unity-buffer-name-nondirectory-or-test-file test-file)))
    (if (unity-is-header-file-p file-name)
        (setq file-name
              (unity-switch-file-name file-name "header-to-src")))
    (unity-new-find-and-switch-buffer file-name
                                      unity-temp-sensor-project-list test-file)))

(defun unity-cycle-alpha-ascending ()
  (interactive)
  (unity-cycle-alphabetic-group buffer-file-name "ascending")
  )

(defun unity-cycle-alpha-descending ()
  (interactive)
  (unity-cycle-alphabetic-group buffer-file-name "descending")
  )

(defun unity-cycle-alphabetic-group
  (file-name search-direction &optional test)
  "Cycle between files alphabetically contained
   within current buffer directory."

  (let ((idx
         (unity-index-current-buffer file-name t)))
    (let (( return-val nil))
      (cond ((equal search-direction "ascending")
             (setq return-val
                   (unity-search-buffer
                    idx file-name "higher" search-direction test))
             (if(not return-val)
                 (setq return-val (unity-search-buffer 
                                   idx file-name "lower"
                                   search-direction test))))
            ((equal search-direction "descending")
             (setq return-val
                   (unity-search-buffer
                    idx file-name "lower" search-direction test))
             (if(not return-val)
                 (setq return-val
                       (unity-search-buffer
                        idx file-name "higher" search-direction test))))
            ( t (unity-error-with-param
                 "Invalid search-direction"
                 search-direction
                 "in unity-cycle-alphabetic-group")))
      return-val)))

(defun unity-alpha-start-index (search-direction higher-lower index)
  (cond ((equal search-direction "ascending")
         (cond ((equal higher-lower "higher")
                index)
               ((equal higher-lower "lower" )
                0)
               ( t (unity-error-with-param
                    "Invalid search-direction"
                    search-direction
                    "in unity-search-buffer"))))

        ((equal search-direction "descending")
         (cond ((equal higher-lower "higher") 
                (length files))
               ((equal higher-lower "lower" )
                index)
               ( t (unity-error-with-param
                    "Invalid search-direction"
                    search-direction
                    "in unity-get-index-limit"))))
        (t (unity-error-with-param
            "Invalid search-direction"
            search-direction
            "in unity-get-index-limit"))))

(defun unity-search-buffer
  (index file-name higher-lower search-direction  &optional test)
  (let ((files
         (directory-files 
          (file-name-directory file-name)))
        (ext (file-name-extension file-name))
        (searching t))

    (let ((j (unity-alpha-start-index
              search-direction higher-lower index)))

      (let ((result (nth j files)))
        
        (while searching
          (setq j
                (cond ((equal search-direction "ascending")
                       (+ j 1))
                      ((equal search-direction "descending")
                       (- j 1))
                      (t (unity-error-with-param
                          "Invalid search-direction"
                          search-direction
                          "in unity-search-buffer"))))
          (setq result (nth j files))
          (if(and
              (cond ((equal higher-lower "lower")
                     (cond ((equal search-direction "ascending")
                            (if (>= j index)
                                (progn
                                  (setq result t)
                                  (setq searching nil)
                                  nil)
                              t))
                           ((equal search-direction "descending")
                            (if (<= j 0)
                                (progn
                                  (setq result nil)
                                  (setq searching nil)
                                  nil)
                              t))
                           (t (unity-error-with-param
                               "Invalid search-direction"
                               search-direction
                               "in unity-search-buffer"))))
                    
                    ((equal higher-lower "higher")
                     (cond ((equal search-direction "descending")
                            (if (<= j index)
                                (progn
                                  (setq result t)
                                  (setq searching nil)
                                  nil)
                              t))
                           (t)t))
                    (t (unity-error-with-param
                        "Invalid search-direction"
                        search-direction
                        "in unity-search-buffer")))
              
              (if(not result)
                  (progn
                    (setq searching nil)
                    nil)
                t)
              (not (file-directory-p result))
              (not(string-match"^\\." result)))

              (if(equal (file-name-extension result) ext)
                  (progn
                    (message (concat "Switching to "
                                     result))
                    (setq searching nil)
                    (if(and(not test)
                           result)
                        (find-file
                         result))))))
        result))))

(defun unity-get-list(index pattern)
  "Returns list present at this INDEX of
this PATTERN"
  (car(last(car(cdr(assoc index pattern))))))

(defun unity-is-valid-pattern-p (index pattern)
  "Returns true if INDEX indicates a valid pattern
list in PATTERN"
  (if (unity-get-list index pattern) t nil))

(defun unity-try-pattern-option(index option pattern)
  "Returns pattern option found at this index or nil
if none found"
  (nth option (unity-get-list index pattern)))

(defun unity-get-current-pattern-index (match pattern)
  "Returns index of current file-string pattern if
it is a pattern file, else nil
MATCH is the pattern as string to match with
PATTERN is the pattern to use"
  (let ((i 0)
        (list nil)
        (finished nil)
        (suggestion nil)
        (option-count 0))
    (while(not finished)
      (setq suggestion (unity-try-pattern-option i option-count pattern))
      (if suggestion
          (if(equal suggestion match)
              (setq finished t)
            (setq option-count (+ option-count 1)))
        (progn ;suggestion == nil
          (setq i (+ i 1)) ; next list
          (if (unity-is-valid-pattern-p i pattern) ;valid list?
              (setq option-count 0)
            (setq finished t))))) 
    (if suggestion i nil)))

(defun unity-extract-pattern-index-from-file-name (file-name pattern-list)
  "Returns index of current file-string pattern if
it is a pattern file, else nil
MATCH is the pattern as string to match with
PATTERN is the pattern to use"
  (let ((i 0)
        (list nil)
        (finished nil)
        (suggestion nil)
        (option-count 0))
    (while(not finished)
      (setq suggestion (unity-try-pattern-option i option-count pattern-list))
      (if suggestion
          (if (unity-is-pattern-in-file-name-p file-name suggestion)
              (setq finished t)
            (setq option-count (+ option-count 1)))
        (progn ;suggestion == nil
          (setq i (+ i 1)) ; next list
          (if (unity-is-valid-pattern-p i pattern-list) ;valid list?
              (setq option-count 0)
            (setq finished t))))) 
    (if suggestion i nil)))

(defun unity-is-pattern-in-file-name-p (file-name pattern)
  (unity-string-exact-match
   (concat
    "^.*"
    pattern
    "\\"
    unity-src-file-extension
    "$")
   (file-name-nondirectory file-name)))

(defun unity-get-target-dir(file-name)
  (if (unity-is-test-file-p file-name)
      unity-test-dir
    unity-src-dir))

(defun unity-new-find-and-switch-buffer (file-name patterns &optional test)
  (let ((current-index
         (unity-extract-pattern-index-from-file-name file-name unity-temp-sensor-project-list))
        (finished nil) 
        (return-index t)
        (upper-search t))
    (let ((current-pattern
           (unity-read-pattern-in-file-name
            file-name
            current-index
            patterns)))
      (let ((name
             (unity-new-read-name
              file-name
              current-pattern)))
        (let ((i (+ 1 current-index))
              (option-count 0))
          (while(not finished) 
            (if(not (unity-is-valid-pattern-p i patterns))
                (progn
                  (setq upper-search nil)
                  (setq i 0))
              (let ((new-pattern
                     (unity-try-pattern-option
                      i
                      option-count
                      patterns)))
                (if new-pattern
                    (let ((new-name
                           (unity-build-pattern-file-name
                            file-name
                            current-pattern
                            new-pattern)))
                      (let ((file-with-path
                             (concat (unity-get-target-dir
                                      new-name)
                                     new-name))) 
                        (if (file-exists-p file-with-path)
                            (progn
                              (setq finished t)
                              (if (not test)
                                  (find-file file-with-path)
                                (setq file-name new-name))))
                        (setq option-count (+ 1 option-count))))
                  (progn
                    (setq option-count 0)
                    (setq i (+ i 1))
                    (if (not upper-search)
                        (if (>= i current-index)
                            (progn
                              (setq file-name nil)
                              (setq finished t)))))))))))))
  file-name)

(defun unity-build-pattern-file-name (file-name old-pattern new-pattern)
  (let ((ext (file-name-extension file-name)))
    (let ((file-name
           (unity-replace-regex-in-string
            (concat old-pattern "$")
            new-pattern
            (file-name-sans-extension file-name))))
      (concat file-name "." ext))))

(defun unity-new-read-name (file-name pattern)
  (unity-replace-regex-in-string
   (concat pattern "$")
   ""
   (file-name-sans-extension file-name)))

(defun unity-read-pattern-in-file-name (file-name index pattern-list)
  (let ((i 0)
        (finished nil)
        (pattern nil))
    (while(not finished)
      (setq pattern
            (unity-try-pattern-option index i pattern-list))
      (if(or
          (not pattern)
          (unity-is-pattern-in-file-name-p file-name pattern))
          (setq finished t)
        (setq i (+ i 1))))
    pattern))

(provide 'unity-mode)

(defun unity-test (file-name patterns &optional test)
  (let ((current-index
         (unity-extract-pattern-index-from-file-name file-name unity-temp-sensor-project-list))
        (finished nil) 
        (return-index t)
        (upper-search t))
    (let ((current-pattern
           (unity-read-pattern-in-file-name
            file-name
            current-index
            patterns)))
      (let ((name
             (unity-new-read-name
              file-name
              current-pattern)))
        (let ((i (+ 1 current-index))
              (option-count 0))
          (while(not finished) 
            (if(not (unity-is-valid-pattern-p i patterns))
                (progn
                  (setq upper-search nil)
                  (setq i 0))

              (let ((new-pattern
                     (unity-try-pattern-option
                      i
                      option-count
                      patterns)))
                (if new-pattern
                    (let ((new-name
                           (unity-build-pattern-file-name
                            file-name
                            current-pattern
                            new-pattern)))
                      (let ((file-with-path
                             (concat (unity-get-target-dir
                                      new-name)
                                     new-name))) 
                        (if (file-exists-p file-with-path)
                            (progn
                              (setq finished t)
                              (if (not test)
                                  (find-file file-with-path)
                                (setq file-name new-name))))
                        (setq option-count (+ 1 option-count))))
                  (progn
                    (setq option-count 0)
                    (setq i (+ i 1))
                    (if (not upper-search)
                        (if (>= i current-index)
                            (progn
                              (setq file-name nil)
                              (setq finished t))))))))))))))
 
(defun unity-get-primative (order)
  "Return primative list for given ORDER. Return nil if doesn't exist"
  (if (< order 0)
      (unity-error-with-param "Negative order" order "unity-get-primative")
    (nth order  unity-temp-sensor-primatives-list)))
  
;;'('test t 0 '("Test test_") '("") '("c" "C") '("Test/" "test/") )

(defun unity-get-next-primative (last-primative, direction)
"Return next primative list relative to LAST-PRIMATIVE, where
DIRECTION is 'up or 'down and CURRENT-PRIMATIVE is an ORDER (intitally
nil). If switch? in list is nil looks for next primative list. Returns nil if
no primatives found."
)

(setq unity-primative-elements
      '('name 'switch? 'order 'prefixes 'suffixes 'extensions 'locations))

(defun get-primative-element-for-idx (count)
  (cadr (nth count unity-primative-elements)))

(defun unity-get-primative-element (element list)
"Returns primative-element in LIST. ELEMENT is a symbol representing the element in question"

(let ((return-val nil)
      (list-length (length unity-primative-elements))
      (count 0))
  (while (and (< count list-length)
              (not return-val))
    (when (equal
           element
           (get-primative-element-for-idx count))
      (setq return-val (nth count list)))
    (setq count (+ count 1)))
  return-val)
)
  
