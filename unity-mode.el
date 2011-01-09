;;; unity-mode.el --- minor mode for Unity, CMock, and Ceedling
;; unit-testing, mocking, configuration integration.

(defconst unity-mode-abbrev-table (make-abbrev-table))

(defconst unity-mode-keymap (make-sparse-keymap) "Keymap used in unity mode")

(define-key unity-mode-keymap (kbd "C-; r") 'unity-test)
(define-key unity-mode-keymap (kbd "C-; o") 'unity-test-only)
(define-key unity-mode-keymap (kbd "C-; a") 'unity-test-all)
(define-key unity-mode-keymap (kbd "C-; d") 'unity-test-delta)
(define-key unity-mode-keymap (kbd "C-; i") 'unity-toggle-test-ignored)
(define-key unity-mode-keymap (kbd "C-; t") 'unity-toggle-test-and-target)
(define-key unity-mode-keymap (kbd "C-; C-h") 'unity-toggle-src-and-header)
(define-key unity-mode-keymap (kbd "C-; m") 'unity-toggle-triad)
(define-key unity-mode-keymap (kbd "C-; n") 'unity-new-menus)

(defvar unity-mode-map nil) ; No local map
(setq unity-mode-results-map (make-sparse-keymap))
(define-key unity-mode-results-map "\r" 'unity-goto-location)
(define-key unity-mode-results-map [mouse-2] 'unity-goto-location)

(defgroup unity-mode nil
  "Unity minor mode.")

(defvar unity-test-file-prefix "test_")

(defvar unity-mock-file-prefix "mock_")

(defvar unity-source-file-extension ".c")
(setq unity-prefix "Test-")

(defvar unity-header-file-extension ".h")
(setq unity-prefix "Test-")

(defcustom unity-rake-command "rake"
  "The command for rake"
  :type 'string
  :group 'unity-mode)

(defcustom unity-project-root-dir "/home/martyn/ceedling5/trunk/examples/temp_sensor/"
  "Project Root Directory"
  :type 'string
  :group 'unity-mode)

;;;###autoload
(define-minor-mode unity-mode
"Enhanced C-mode for Unity, CMock, and Ceedling
Unit testing integration"
  :lighter "unity"
  :keymap  unity-mode-keymap)

(defun unity-is-a-test-file-p (file-name)
  "Returns true if the file is a test file"
  (interactive)
  (string-match "[Tt]est.*\\.[cC]$" file-name))

(defun unity-is-a-header-file-p (file-name)
  "Returns true if the file is a header file"
  (interactive)
  (string-match ".*\\.[hH]$" file-name))

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
    (concat file-name unity-source-file-extension)))

(defun unity-create-source-file-name (file-name)
  "Returns file-name but converted into a non-test (source) file name"
     (concat (file-name-directory file-name)
             (unity-c-file-extension-if-necessary 
              (replace-regexp-in-string
               unity-test-file-prefix
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
   unity-header-file-extension
   unity-source-file-extension
   file-name))

(defun unity-src-to-header-file-name (file-name)
  "Find the header file for a source file"
  (replace-regexp-in-string
   unity-source-file-extension
   unity-header-file-extension
   file-name))

(defun unity-find-src-for-test-file (test-file-name)
  "Find the target for test-file-name"
  (first
   (file-expand-wildcards
        (replace-regexp-in-string
         "/test/"
         (if (unity-test-src-file-p test-file-name) "/" "/*/")
         (unity-create-source-file-name test-file-name)))))

(defun unity-find-test-for-src-file (file-name)
  "Find test for the testified file"
  (if (unity-is-a-test-file-p file-name)
      file-name
    (let ((replace-regex
           (if
               (and
                (unity-target-src-file-p file-name)
                (unity-test-directory-has-src-p file-name))
               "^\\.\\./"
             "^\\.\\./[^/]+/"))
          (relative-file-name
           (file-relative-name file-name
                               (unity-test-directory file-name))))
      (unity-testize-file-name
       (expand-file-name
        (replace-regexp-in-string replace-regex "" relative-file-name)
        (unity-test-directory file-name))))))



(defun unity-target-src-file-p (file-name)
  "Returns"
  (string-match
   (concat "^" (expand-file-name
                (regexp-quote
                 (concat
                  (unity-project-root file-name) "/src")))) file-name))

(defun unity-test-directory-has-src-p (file-name)
  "Returns t if /src is found in appropriate place" 
  (file-directory-p (concat (unity-test-directory file-name) "/src")))

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

(defun unity-verify ()
  "Runs the specified test, or the test file for the current buffer."
  (interactive)
  (unity-run-single-file (unity-find-test-for-src-file (buffer-file-name)) (unity-core-options ())))

;;;###autoload
(defun unity-buffer-is-test-p ()
  "Returns true if the current buffer is a test file"
  (and (buffer-file-name)
       (unity-is-a-test-file-p (buffer-file-name))))

;;;###autoload
(defun unity-buffer-is-header-p ()
  "Returns true if the current buffer is a header file"
  (and (buffer-file-name)
       (unity-is-a-header-file-p (buffer-file-name))))

(defun unity-source-directory-has-src? (file-name)
  (file-directory-p (concat (unity-source-directory file-name) "/src")))

(defun unity-test-directory (file-or-directory)
  "Returns the nearest test directory that could contain tests for file-or-directory. The function is passed either a file or directory, and is recursive."
  (if (file-directory-p file-or-directory)
      (or
       (first (directory-files file-or-directory t "^test$"))
       (if (unity-root-directory-p file-or-directory)
           nil
         (unity-test-directory (unity-parent-directory file-or-directory))))
    (unity-test-directory (unity-parent-directory file-or-directory))))

(defun unity-test-src-file-p (test-file-name)
  "Returns t if passed a valid path to a C source file"
  (string-match
   (concat
    "^"
    (expand-file-name
     (regexp-quote
      (concat
       (unity-project-root test-file-name)
       "/src")))) test-file-name))

(defun unity-project-root (&optional directory)
  "Finds the root directory of the project by walking the directory tree until it finds a rake file."
  (interactive)
  (let ((directory (file-name-as-directory (or directory default-directory))))
    (cond ((unity-root-directory-p directory) nil)
          ((file-exists-p (concat directory "rakefile.rb")) directory)
          (t (unity-project-root (file-name-directory (directory-file-name directory)))))))
                                    
;; (defun test-unity-is-a-header-file-p-detects-header ()
;;   (interactive)
;;   (if (unity-is-a-header-file-p "header.h")
;;       (message "File detected as header")
;;     (message "ERROR! Header not detected as file")))

;; (defun test-unity-is-a-header-file-p-detects-non-header ()
;;   (interactive)
;;   (if (unity-is-a-header-file-p "non_header.c")
;;       (message "ERROR!File detected as header")
;;     (message" Header not detected as expected")))
                         
;; (defun test-unity-buffer-is-header-p ()
;;   (interactive)
;;   (if (unity-buffer-is-header-p)
;;       (message "Current buffer is header file")
;;     (message "Current buffer is NOT header file")))

  (defun unity-toggle-test-and-target ()
  "Toggle test and target buffers"
  (interactive)
  (if (unity-buffer-is-test-p)
      (if (unity-test-src-file-p
           (unity-find-src-for-test-file (buffer-file-name)))
          (find-file (unity-find-src-for-test-file (buffer-file-name)))
        (message "isn't test file"))
    (if (unity-is-a-test-file-p
         (unity-find-test-for-src-file (buffer-file-name)))
        (find-file (unity-find-test-for-src-file (buffer-file-name)))
      (if (unity-buffer-is-header-p)
          (if (unity-is-a-test-file-p
               (unity-find-test-for-src-file
                (unity-header-to-src-file-name
                 (buffer-file-name))))
              (find-file
               (unity-find-test-for-src-file
                (unity-header-to-src-file-name
                 (buffer-file-name)))))
        (error "Couldn't find matching file")))))
        
  (defun unity-toggle-src-and-header ()
  "Toggle between source file and header file"
  (interactive)
  (if (unity-buffer-is-header-p)
      (if (unity-test-src-file-p
           (unity-header-to-src-file-name (buffer-file-name)))
          (find-file (unity-header-to-src-file-name (buffer-file-name)))
        (error "Couldn't find matching file"))
    (if (unity-test-src-file-p (unity-src-to-header-file-name (buffer-file-name)))
        (find-file (unity-src-to-header-file-name (buffer-file-name)))
      (if (unity-buffer-is-test-p)
          (if (unity-test-src-file-p
               (unity-src-to-header-file-name
                 (unity-find-src-for-test-file (buffer-file-name))))
              (find-file (unity-src-to-header-file-name
                          (unity-find-src-for-test-file (buffer-file-name)))))
      (error "Couldn't find matching file")))))

(defun test-unity-header-to-src-file-name-with-small-h ()
  (interactive)
  (message (unity-header-to-src-file-name "my_header.h")))

(defun test-unity-header-to-src-file-name-with-capital-H ()
  (interactive)
  (message (unity-header-to-src-file-name "my_header.H")))

(defun test-unity-src-to-header-file-name-with-small-h ()
  (interactive)
  (message (unity-src-to-header-file-name "/home/martyn/etc/src/source.c")))

(defun unity-headerize-file-name (file-name)
  "Returns a file name converted into header file name"
  (unity-src-to-header-file-name file-name))

(defun test-unity-headerize-file-name ()
  (interactive)
  (message (unity-headerize-file-name (buffer-file-name))))

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

(defvar unity-buffer-name "*Unity-Test*")

(defun unity-run-file ()
  "Run buffer's file as test, first visible window file or
last-run as ruby test (or spec)."
  (interactive)
  (setq unity-buffer (get-buffer-create unity-buffer-name))
;;  (let ((test-file (find-unity-file)))
  (let ((test-file "/home/martyn/ceedling5/trunk/examples/temp_sensor/test/TestAdcConductor.c"))
    (if t ;; test-file
	(unity-run-test-file test-file unity-buffer)
      (message unity-not-found-message))))

(defun unity-run-test-file (file output-buffer &optional line-number)
  (let (command category (options (list file)))
    ;; (cond
    ;;  ((ruby-spec-p file) 
    ;;   (setq command (or (ruby-test-spec-executable test-file) spec))
    ;;   (setq category "spec")
    ;;   (if line-number
    ;;       (setq options (cons "--line" (cons (format "%d" line-number) options)))))
    ;;  ((ruby-test-p file)
    ;;   (setq command (or (ruby-test-ruby-executable) "ruby"))
    ;;   (setq category "unit test")
    ;;   (if line-number
    ;;       (let ((test-case (ruby-test-find-testcase-at file line-number)))
    ;;         (if test-case
    ;;     	(setq options (cons file (list (format "--name=%s" test-case))))
    ;;           (error "No test case at %s:%s" file line-number)))))
    ;;   (t (message "File is not a known ruby test file")))

    (if (unity-is-a-test-file-p file) 
;;       (setq command (or (ruby-test-spec-executable test-file) spec))
       (setq command ";;")
;;       (setq category "spec")
;;       (if line-number
;;           (setq options (cons "--line" (cons (format "%d" line-number) options))
      (error "File not a test file"))
    (invoke-test-file  file output-buffer)))

;; (defun invoke-test-file (command-string options category file buffer)
;;   (message "Running %s '%s'..." category file)
;;   (display-buffer buffer)
;;   (setq ruby-test-last-run file)
;;   (save-excursion
;;     (set-buffer buffer)
;;     (setq buffer-read-only t)
;;     (let ((buffer-read-only nil))
;;       (erase-buffer)
;;       (set-auto-mode-0 'ruby-test-mode nil)
;;       (let ((args (append (list command-string) options)))
;;         (let ((directory (ruby-root file))
;; 	      (previous-directory default-directory))
;;           (and directory (cd directory))
;; 	  (let ((proc (apply 'start-process "ruby-test" buffer args)))
;; 	    (set-process-sentinel proc 'ruby-test-runner-sentinel))
;; 	  (and directory (cd previous-directory)))))))

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

(defvar unity-test-ok-message
  (progn
    (let ((msg "OK"))
      (put-text-property 0 2 'face '(foreground-color . "dark green") msg)
      msg)))

(defvar unity-test-fail-message
  (progn
    (let ((msg "Failed"))
      (put-text-property 0 6 'face '(foreground-color . "red") msg)
      msg)))

(defvar unity-test-fail-message-with-reason
  (progn
    (let ((msg "Failed: '%s'"))
      (put-text-property 0 6 'face '(foreground-color . "red") msg)
      msg)))

(defun unity-runner-sentinel (process event)
  (save-excursion
    (set-buffer unity-buffer)
    (cond
     ((string= "zzzfinished\n" event) (message unity-test-ok-message))
     ((string= "exited abnormally with code 1\n" event) (message unity-test-fail-message))
     (t (progn
	  (string-match "\\(.*\\)[^\n]" event)
	  (message unity-test-fail-message-with-reason (match-string 1 event)))))))

(defun xx ()
  (interactive)
  (unity-run-file))

(provide 'unity-mode)

;;
;; Unit Tests...
;;

(ert-deftest unity-assert-test ()
  (should t))

(ert-deftest unity-not-assert-test ()
  (should-not nil))

(ert-deftest unity-check-constants ()
  "Checks constants are defined"
  (should (equal unity-mode-keymap unity-mode-keymap)))

(ert-deftest unity-check-unity-buffer-name-assignment ()
  (should (equal unity-buffer-name "*Unity-Test*")))

(ert-deftest unity-check-unity-test-file-prefix-assignment ()
  (should (equal unity-test-file-prefix "test_")))

(ert-deftest unity-check-unity-mock-file-prefix-assignment ()
  (should (equal unity-mock-file-prefix "mock_")))

(ert-deftest unity-source-file-extension-assignment ()
  (should (equal unity-source-file-extension ".c")))

(ert-deftest unity-header-file-extension-assignment ()
  (should (equal unity-header-file-extension ".h")))

(ert-deftest unity-is-a-test-file-p-returns-correct-result ()
  (should (unity-is-a-test-file-p "test_file.c"))
  (should-not (unity-is-a-test-file-p "test_file.h"))
  (should-not (unity-is-a-test-file-p "file.c")))

(ert-deftest unity-is-a-header-file-p-returns-correct-result ()
  (should (unity-is-a-header-file-p "test_file.h"))
  (should-not (unity-is-a-header-file-p "file.c"))
  (should (unity-is-a-header-file-p "file.h")))

(ert-deftest unity-is-a-header-file-p-returns-correct-result ()
  (should (unity-is-a-header-file-p "test_file.h"))
  (should-not (unity-is-a-header-file-p "file.c"))
  (should (unity-is-a-header-file-p "file.h")))

(ert-deftest unity-parent-directory-returns-correct-directory-or-nil ()
  (should (equal "~/parent/"
            (unity-parent-directory "~/parent/child/")))
  (should (equal "~/grand-parent/parent/"
            (unity-parent-directory "~/grand-parent/parent/child/")))
  (should (equal "/"
                 (unity-parent-directory "/"))))

(ert-deftest unity-root-directory-p-returns-correct-result ()
  (should-not (unity-root-directory-p "~/test/"))
  (should-not (unity-root-directory-p "~/"))
  (should (unity-root-directory-p "/"))
  (should-not (unity-root-directory-p "")))

(ert-deftest unity-c-file-extension-if-necessary-returns-correct-file-name ()
  (setq unity-source-file-extension ".c")
  (should (equal "test_file.c"
                 (unity-c-file-extension-if-necessary "test_file.c")))
  (should (equal "test_file.c"
                 (unity-c-file-extension-if-necessary "test_file")))
  (should (equal "test_file.h"
                 (unity-c-file-extension-if-necessary "test_file.h")))
  (setq unity-source-file-extension ".cpp")
  (should (equal "test_file.c"
                 (unity-c-file-extension-if-necessary "test_file.c")))
  (should (equal "test_file.cpp"
                 (unity-c-file-extension-if-necessary "test_file")))
  (should (equal "test_file.h"
                 (unity-c-file-extension-if-necessary "test_file.h")))
  (setq unity-source-file-extension ".c"))

(ert-deftest unity-create-source-file-name-returns-correct-file-name ()
  (should (equal "file_name.c"
                 (unity-create-source-file-name "test_file_name.c")))
  (should (equal "file_name.c"
                 (unity-create-source-file-name "test_file_name")))
  (should (equal "~/file_name.c"
                 (unity-create-source-file-name "~/test_file_name.c"))))

(ert-deftest unity-create-test-file-name-returns-correct-file-name ()
  (should (equal "test_file_name.c"
                 (unity-create-test-file-name "file_name.c")))
  (should (equal "test_file_name.c"
                 (unity-create-test-file-name "file_name")))
  (should (equal "~/test_file_name.c"
                 (unity-create-test-file-name "~/file_name.c"))))

(ert-deftest unity-header-to-src-file-name-returns-correct-file-name ()
  (should (equal "file_name.c"
                 (unity-header-to-src-file-name "file_name.h")))
  (should (equal "~/file_name.c"
                 (unity-header-to-src-file-name "~/file_name.h")))
  (should (equal "file_name.c"
                 (unity-header-to-src-file-name "file_name.c"))))

(ert-deftest unity-src-to-header-file-name-returns-correct-file-name ()
  (should (equal "file_name.h"
                 (unity-src-to-header-file-name "file_name.c")))
  (should (equal "~/file_name.h"
                 (unity-src-to-header-file-name "~/file_name.c")))
  (should (equal "file_name.h"
                 (unity-src-to-header-file-name "file_name.h"))))

;; (ert-deftest unity-find-src-for-test-file-correct-whatever ()
;;   (should (equal ""
;;                  (unity-find-src-for-test-file "test_file_1.c"))))
