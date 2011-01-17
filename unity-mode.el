;;; unity-mode.el --- minor mode for Unity, CMock, and Ceedling
;; unit-testing, mocking, configuration integration.

(require 'unity-auto-config)
(require 'unity-rakefile)
(require 'unity-tests)

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
  (kbd "C-; r") 'unity-test)
(define-key unity-mode-keymap
  (kbd "C-; o") 'unity-test-only)
(define-key unity-mode-keymap
  (kbd "C-; a") 'unity-test-all)
(define-key unity-mode-keymap
  (kbd "C-; d") 'unity-test-delta)
(define-key unity-mode-keymap
  (kbd "C-; n") 'unity-new-menus)

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

(defcustom unity-rake-command "rake"
  "The command for rake"
  :type 'string
  :group 'unity-mode)
(defcustom unity-project-root-dir
  "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/\
examples/temp_sensor/" "Project Root Directory"
:type 'string
:group 'unity-mode)
(defcustom unity-ceedling-root-dir
  "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/"
  "Ceedling Root Directory"
  :type 'string
  :group 'unity-mode)
(defcustom unity-unity-root-dir
  "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/\
vendor/unity/" "Unity Root Directory"
:type 'string
:group 'unity-mode)
(defcustom unity-cmock-root-dir
  "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/\
vendor/cmock/" "CMock Root Directory"
:type 'string
:group 'unity-mode)
(defcustom unity-plugins-dir
  "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/\
plugins/" "Plugins Directory"
:type 'string
:group 'unity-mode)
(defcustom unity-custom-plugins-dir
  "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/\
custom_plugins/" "Custom Plugins Directory"
:type 'string
:group 'unity-mode)
(defcustom unity-src-dir
  "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/\
examples/temp_sensor/src/" "Source Directory"
:type 'string
:group 'unity-mode)
(defcustom unity-test-dir
  "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/\
examples/temp_sensor/test/" "Test Files Directory"
:type 'string
:group 'unity-mode)
(defcustom unity-header-dir
  "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/\
examples/temp_sensor/src/" "Header Files Directory"
:type 'string
:group 'unity-mode)
(defcustom
  unity-mocks-dir
  "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/\
examples/temp_sensor/mocks" "Mock Files Directory"
:type 'string
:group 'unity-mode)
(defcustom
  unity-build-dir
  "/home/martyn/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/\
examples/temp_sensor/build/"
  "Build Files Directory"
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
  (concat
   "Conductor File Suffix (as in some[unity-conductor-file-suffix].c)")
  :type 'string
  :group 'unity-mode)
(defcustom unity-hardware-file-suffix "Hardware"
  (concat
   "Hardware File Suffix (as in some[unity-hardware-file-suffix].c)")
  :type 'string
  :group 'unity-mode)
(defcustom unity-configurator-file-suffix "Configurator"
  (concat
   "Configurator File Suffix (as in some[unity-configurator-file-suffix].c)")
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

(defun dbg(msg)
  (if (not msg)
      (setq msg "nil"))
  (if (equal msg t)
      (setq msg "t"))
  (error (concat "dbg: " msg))) 

(defun unity-replace-regex-in-string (regex replaced string)
  (let ((case-fold-search nil)
        (case-replace nil))
    (replace-regexp-in-string
     regex replaced string t t nil nil)))

(defun unity-file-prefix (file-type)
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
             "Invalid file-type")
            file-type
            "in unity-file-prefix")))

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

(defun unity-file-extension (file-type)
  (cond ((equal file-type "header-file")
         unity-header-file-extension)
        ((or (equal file-type "src-file")
             (equal file-type "test-file")
             (equal file-type "model-file")
             (equal file-type "conductor-file")
             (equal file-type "hardware-file"))
         unity-src-file-extension)
        ( t (unity-error-with-param
             "Invalid file-type"
             file-type
             "in unity-file-extension!"))))

(defun unity-strip-affixes (file-name)
  (unity-strip-affix
   (unity-strip-affix
    file-name "suffix-type")
   "prefix-type"))

(defun unity-strip-affix (file-name affix-type)
  "remove defined affix from file-name"
  (cond ((equal affix-type "prefix-type")
         (unity-strip-affix-core file-name affix-type))
        ((equal affix-type "suffix-type")
         (operate-sub-extension 'unity-strip-affix-core
                                file-name "suffix-type"))
        ( t (unity-error-with-param
             "Invalid affix-type"
             affix-type
             "in unity-strip-affix"))))

(defun unity-strip-affix-core (file-name affix-type)
  (let ((regex1
         (cond ((equal affix-type "prefix-type")
                "^")
               ((equal affix-type "suffix-type")
                "")))
        (regex2
         (cond ((equal affix-type "prefix-type")
                "")
               ((equal affix-type "suffix-type")
                "$")))
        (list 
         (cond ((equal affix-type "prefix-type")
                (unity-file-prefix-list))
               ((equal affix-type "suffix-type")
                (unity-file-suffix-list)))))

    (mapcar (lambda (x)
              (setq file-name
                    (unity-replace-regex-in-string
                     (concat regex1 x regex2)
                     ""
                     file-name)))
            list)
    file-name))

(defun unity-read-name (file-name)
  (unity-strip-affixes
   (file-name-nondirectory 
    (file-name-sans-extension
     file-name))))

(defun unity-string-exact-match (regex string &optional start)
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


;; (unity-string-exact-match
;;  (concat
;;   "^"
;;   (unity-file-prefix file-type)
;;   (error (unity-read-name file-name file-type))
;;   (unity-file-suffix file-type)
;;   "\\"

;;   (unity-file-extension file-type)
;;   "$")

;;  (file-name-nondirectory file-name))))

(defun unity-is-test-file-p (file-name)
  "Returns true if the file is a test file"
  (if (equal (unity-read-extension file-name)
             unity-src-file-extension)
      (unity-check-prefix-p file-name "test-file")
    nil))

;;  (unity-is-file-type-p file-name "test-file"))

(defun unity-is-src-file-p (file-name)
  "Returns t if filename has a C extension and is not a testfile"
;  (dbg (unity-check-prefix-p file-name "src-file"));
;  (dbg  (unity-check-extension-p file-name "src-file"))
  (and (unity-check-prefix-p file-name "src-file")
       (unity-check-extension-p file-name "src-file")))
;TODO  (unity-is-file-type-p file-name "src-file"))

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

(defun unity-has-file-suffix-p (file-name file-type)
  (let ((suffix
         (unity-file-suffix file-type)))
    
    (unity-string-exact-match
     (concat
      "^.*"
      suffix
      "\\.*")
     (file-name-nondirectory file-name))))

(defun unity-is-model-file-p (file-name)
  "Returns true if FILE-NAME is a model file"
  ;;  (unity-is-file-type-p file-name "model-file"))
  (unity-string-exact-match
   (concat
    "^.*"
    unity-model-file-suffix
    "\\"
    unity-src-file-extension
    "$")
   (file-name-nondirectory file-name)))

(defun unity-is-conductor-file-p (file-name)
  "Returns true if FILE-NAME is a conductor file"
  ;;(unity-is-file-type-p file-name "conductor-file")
  (unity-string-exact-match
   (concat
    "^.*"
    unity-conductor-file-suffix
    "\\"
    unity-src-file-extension
    "$")
   (file-name-nondirectory file-name)))

(defun unity-is-hardware-file-p (file-name)
  "Returns true if FILE-NAME is a hardware file"
  ;;  (unity-is-file-type-p file-name "hardware-file"))
  (unity-string-exact-match
   (concat
    "^.*"
    unity-hardware-file-suffix
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

(defun unity-file-exists-p (file-name file-type toggle-type)
  "Determines whether the argument FILE-NAME is present in the
unity directories.

Second argument FILE-TYPE is a string containing file type
such as \"str-type\.

Third argument TOGGLE-TYPE is a string containing toggle type
such as \"mch-type\"."
  (file-exists-p
   (concat
    (unity-select-path
     file-name
     file-type
     toggle-type)
    file-name)))

(defun unity-dest-file-type (switch-type)
  (concat
   (unity-replace-regex-in-string "^.*-" "" switch-type)
   "-type"))

(defun unity-select-path (file-name file-type toggle-type)
  (cond ((equal toggle-type "non-mch-type")
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
        ((equal toggle-type "mch-type")
         (cond ((unity-is-test-file-p file-name)
                unity-test-dir)
               (t unity-src-dir)))
        (t (unity-error-with-param
            "Invalid toggle-type"
            toggle-type
            "in unity-select-path"))))

(defun unity-switch-buffer (file-name switch-type toggle-type &optional test)
  (let (
        (temp-name
         (unity-convert-file-name 
          file-name
          switch-type
          toggle-type)))

    (if (unity-file-exists-p
         temp-name
         (unity-dest-file-type switch-type) toggle-type)
        (if (not test)
            (find-file
             (concat
              (unity-select-path
               file-name
               (unity-dest-file-type switch-type)
               toggle-type)
              temp-name))
          temp-name))))

(defun unity-select-replacement-suffix (switch-type)
  (cond ((equal switch-type "conductor-to-model")
         unity-model-file-suffix)
        ((equal switch-type "hardware-to-model")
         unity-model-file-suffix)
        ((equal switch-type "model-to-conductor")
         unity-conductor-file-suffix)
        ((equal switch-type "hardware-to-conductor")
         unity-conductor-file-suffix)
        ((equal switch-type "conductor-to-hardware")
         unity-hardware-file-suffix)
        ((equal switch-type "model-to-hardware")
         unity-hardware-file-suffix)
        (t (unity-error-with-param
            "Invalid switch-type"
            switch-type
            "in  unity-select-replacement-suffix"))))

(defun unity-select-replaced-suffix (switch-type)
  (cond ((equal switch-type "model-to-conductor")
         unity-model-file-suffix)
        ((equal switch-type "model-to-hardware")
         unity-model-file-suffix)
        ((equal switch-type "conductor-to-hardware")
         "Conductor")
        ;;         unity-conductor-file-suffix)
        ((equal switch-type "conductor-to-model")
         unity-conductor-file-suffix)
        ((equal switch-type "hardware-to-conductor")
         unity-hardware-file-suffix
         (unity-replace-regex-in-string
          (concat (unity-select-replaced-suffix switch-type) "\\"
                  unity-src-file-extension "$")
          (concat (unity-select-replacement-suffix switch-type)
                  unity-src-file-extension)
          ;;convert to src if header
          (unity-header-to-src-file-name
           file-name)))))

(defun unity-select-replaced-suffix (switch-type)
  (cond ((equal switch-type "model-to-conductor")
         unity-model-file-suffix)
        ((equal switch-type "model-to-hardware")
         unity-model-file-suffix)
        ((equal switch-type "conductor-to-hardware")
         "Conductor")
        ;;         unity-conductor-file-suffix)
        ((equal switch-type "conductor-to-model")
         unity-conductor-file-suffix)
        ((equal switch-type "hardware-to-conductor")
         unity-hardware-file-suffix)
        ((equal switch-type "hardware-to-model")
         unity-hardware-file-suffix)
        (t (unity-error-with-param
            "Invalid switch-type"
            switch-type
            " in unity-select-replaced-suffix"))))

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

(defun unity-convert-MCH-file-name (file-name switch-type)
  (unity-replace-regex-in-string
   (concat (unity-select-replaced-suffix switch-type) "\\"
           unity-src-file-extension "$")
   (concat (unity-select-replacement-suffix switch-type)
           unity-src-file-extension)
   (unity-header-to-src-file-name file-name)))

(defun unity-convert-file-name (file-name switch-type toggle-type)
  (cond ((equal toggle-type "mch-type")
         (unity-convert-MCH-file-name file-name switch-type))
        ((equal toggle-type "non-mch-type")
         (unity-switch-file-name file-name switch-type))
        (t (unity-error-with-param
            "Invalid toggle-type"
            toggle-type
            "in unity-convert-file-name"))))

(defun unity-cycle-test-src-header-buffer (&optional test test-file)
  "Toggle between test source file "
  (interactive)
  (let ((return nil)
        (file-name
         (if(not test-file)
             (file-name-nondirectory buffer-file-name)
           test-file)))
    
    (cond ((unity-is-test-file-p file-name)
           (if test
               (setq return
               (unity-switch-buffer
                file-name
                "test-to-src"
                "non-mch-type" test))) ;passes true if test
           (if(not
               (unity-switch-buffer
                file-name
                "test-to-src"
                "non-mch-type" test)) ;passes true if test

               (if(not
                   (unity-switch-buffer
                    file-name "test-to-header" "non-mch-type" test))
                   (unity-error "No matching source or header file"))))
          
          ((unity-is-src-file-p file-name)
           (if test
               (setq return
               (unity-switch-buffer
                file-name
                "src-to-header"
                "non-mch-type" test))) ;passes true if test
           (if(not
               (unity-switch-buffer
                file-name
                "src-to-header"
                "non-mch-type" test))
               (if(not
                   (unity-switch-buffer
                    file-name
                    "src-to-test"
                    "non-mch-type"))
                   (unity-error "No matching test or header file" test))))
          
          ((unity-is-header-file-p file-name)
           (if test
               (setq return
               (unity-switch-buffer
                file-name
                "header-to-test"
                "non-mch-type" test))) ;passes true if test
           (if(not
               (unity-switch-buffer
                file-name
                "header-to-test"
                "non-mch-type"))
               (if(not
                   (unity-switch-buffer
                    file-name
                    "header-to-src"
                    "non-mch-type"))
                   (unity-error "No matching src or test file"))))
          ( t (unity-error "File invalid")))
    return))

(defun unity-cycle-MCH-buffer ()
  "Cycle between model conductor and hardware buffers "
  (interactive)
  (let ((file-name
         (file-name-nondirectory buffer-file-name)))
    (cond
     ((unity-is-model-file-p file-name)
      (if(not
          (unity-switch-buffer
           file-name
           "model-to-conductor"
           "mch-type"))
          (if(not
              (unity-switch-buffer
               file-name
               "model-to-hardware"
               "mch-type"))
              (unity-error "No matching conductor or hardware file"))))
     
     ((unity-is-conductor-file-p file-name)
      (if(not
          (unity-switch-buffer
           file-name
           "conductor-to-hardware"
           "mch-type"))
          (if(not
              (unity-switch-buffer
               file-name
               "conductor-to-model"
               "mch-type")
              (unity-error "No matching hardware or model file")))))
     
     ((unity-is-hardware-file-p file-name)
      (if(not
          (unity-switch-buffer
           file-name
           "hardware-to-model"
           "mch-type"))
          (if(not
              (unity-switch-buffer
               file-name
               "hardware-to-conductor"
               "mch-type"))
              (unity-error "No matching model or conductor file"))))
     
     ((unity-is-header-file-p file-name)
      (if(not
          (unity-switch-buffer
           file-name
           "header-to-src"
           "non-mch-type"))
          (if(not
              (unity-switch-buffer
               file-name
               "header-to-test"
               "non-mch-type"))
              (unity-error "No matching file"))))
     (t (unity-error "File invalid")))))

(defun unity-cycle-alphabetic-group (&optional group test)
  "Cycle between files alphabetically contained within current buffer directory.

Argument GROUP can indicate an alternative directory."

  (interactive)
  (let ((file-name (file-name-nondirectory buffer-file-name)))
    (cond ((unity-is-test-file-p file-name)
           (if(not
               (unity-switch-buffer
                file-name
                "test-to-src"
                "non-mch-type"))
               (if (not
                    (unity-switch-buffer
                     file-name "test-to-header" "non-mch-type"))
                   (unity-error "No matching source or header file"))))
          
          ((unity-is-src-file-p file-name)
           (if (unity-search-higher-buffer
                (unity-index-current-buffer file-name)
                "header-to-src")
               (unity-error "File loaded")))
          ( t (unity-error "File invalid")))))

(defun unity-test ()
  (interactive)
  (unity-cycle-alphabetic-group unity-src-dir))

(defun unity-index-current-buffer(file-name)  
  (let ((header-files 
         (directory-files 
          (file-name-directory
           buffer-file-name)))
        (j 0)
        (searching t)
        (result nil))
    
    (while searching
      (setq j (+ 1 j))
      (setq result (nth j header-files))
      (if(or(equal result file-name)
            (equal nil result))
          (setq searching nil)))
    j))

(defun unity-search-higher-buffer(index file-type)
  (let ((header-files 
         (directory-files 
          (file-name-directory
           buffer-file-name)))
        (j 0)
        (searching t)
        (result nil))
    
    (while searching
      (setq j (+ 1 j))
      (setq result (nth j header-files))
      (if (or
           (equal nil result)
           (cond ((unity-is-src-file-p result)
                  (unity-switch-buffer
                   file-name
                   file-type
                   "non-mch-type"))))
          (setq searching nil)))
    j))

(defun unity-string-exact-match (regex string &optional start)
  (let ((case-fold-search nil)
        (case-replace nil))
    (if(and (equal regex "")
            (not(equal string "")))
        nil
      (if (equal 0 (string-match regex string start))
          t
        nil))))

;; (defun unity-cut-extension (filename)
;;   (let ((extension
;;          (file-name-extension filename)))
;;     (if extension

(defun unity-read-extension (file-name)
  (let ((ext nil))
    (setq ext (file-name-extension file-name))
    (cond ((> (length ext) 0)
           (setq ext (concat "." ext)))
          ((not ext)
           (setq ext ""))
          ((equal ext "")
           (setq ext ""))
          (t (unity-error-with-param
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
        (prefix nil)
        (temp nil))
    (mapcar (lambda (x)
              (progn
                (setq temp
                      (string-match
                       (concat "^" x)
                       file-name))
                (if temp
                    (progn
                      (setq prefix x)))))
            (unity-file-prefix-list))
    prefix))
