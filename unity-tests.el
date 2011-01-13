
(provide 'unity-tests)

(defvar unity-test-time '(19750 . 10)) ;;used for ert tests

(ert-deftest unity-assert-test ()
  (should t))

(ert-deftest unity-not-assert-test ()
  (should-not nil))

(ert-deftest unity-check-constants ()
  "Checks constants are defined"
  (should (equal unity-mode-keymap unity-mode-keymap)))

(ert-deftest unity-check-unity-temp-buffer-assignment ()
  (should (equal unity-temp-buffer "*Unity-Buffer*")))

(ert-deftest unity-check-unity-test-file-prefix-assignment ()
  (should (equal unity-test-file-prefix "Test")))

(ert-deftest unity-check-unity-mock-file-prefix-assignment ()
  (should (equal unity-mock-file-prefix "mock_")))

(ert-deftest unity-src-file-extension-assignment ()
  (should (equal unity-src-file-extension ".c")))

(ert-deftest unity-header-file-extension-assignment ()
  (should (equal unity-header-file-extension ".h")))

(ert-deftest unity-ruby-file-extension-assignment ()
  (should (equal unity-ruby-file-extension ".rb")))

(ert-deftest unity-is-test-file-p-returns-correct-result ()
  (should (unity-is-test-file-p "Test_file.c"))
  (should-not (unity-is-test-file-p "Test_file.h"))
  (should-not (unity-is-test-file-p "file.c"))
  (should (unity-is-test-file-p "~/test/Test_file.c"))
  (should-not (unity-is-test-file-p "~/docs/Test_file.ccc")))

(ert-deftest unity-is-src-file-p-returns-correct-result ()
  (should-not (unity-is-src-file-p "Test_file.c"))
  (should-not (unity-is-src-file-p "file.h"))
  (should (unity-is-src-file-p "file.c"))
  (should (unity-is-src-file-p "~/src/src/file.c"))
  (should-not (unity-is-src-file-p "~/build/file.cout")))

(ert-deftest unity-is-header-file-p-returns-correct-result ()
  (should (unity-is-header-file-p "Test_file.h"))
  (should-not (unity-is-header-file-p "file.c"))
  (should (unity-is-header-file-p "file.h"))
  (should (unity-is-header-file-p "~/inc/file.h"))
  (should-not (unity-is-header-file-p "~/docs/file.html")))

(ert-deftest unity-is-a-ruby-file-p-returns-correct-result ()
  (should (unity-is-a-ruby-file-p "file.rb"))
  (should-not (unity-is-a-ruby-file-p "file.c"))
  (should-not (unity-is-a-ruby-file-p "file"))
  (should (unity-is-a-ruby-file-p "~/inc/file.rb"))
  (should-not (unity-is-a-ruby-file-p "~/docs/file.rbcd")))

(ert-deftest unity-is-a-code-file-p-returns-expected ()
  (should (unity-is-a-code-file-p  "~/test/Test_file.c"))
  (should (unity-is-a-code-file-p  "~/src/src_file.c"))
  (should (unity-is-a-code-file-p  "~/inc/header_file.c"))
  (should-not (unity-is-a-code-file-p  "~/docs/some_file.html")))

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
  (setq unity-src-file-extension ".c")
  (should (equal "Test_file.c"
                 (unity-c-file-extension-if-necessary "Test_file.c")))
  (should (equal "Test_file.c"
                 (unity-c-file-extension-if-necessary "Test_file")))
  (should (equal "Test_file.h"
                 (unity-c-file-extension-if-necessary "Test_file.h")))
  (setq unity-src-file-extension ".cpp")
  (should (equal "Test_file.c"
                 (unity-c-file-extension-if-necessary "Test_file.c")))
  (should (equal "Test_file.cpp"
                 (unity-c-file-extension-if-necessary "Test_file")))
  (should (equal "Test_file.h"
                 (unity-c-file-extension-if-necessary "Test_file.h")))
  (setq unity-src-file-extension ".c"))

(ert-deftest unity-create-source-file-name-returns-correct-file-name ()
  (should (equal "file_name.c"
                 (unity-create-source-file-name "Testfile_name.c")))
  (should (equal "file_name.c"
                 (unity-create-source-file-name "Testfile_name")))
  (should (equal "~/file_name.c"
                 (unity-create-source-file-name "~/Testfile_name.c"))))

(ert-deftest unity-create-test-file-name-returns-correct-file-name ()
  (should (equal "Testfile_name.c"
                 (unity-create-test-file-name "file_name.c")))
  (should (equal "Testfile_name.c"
                 (unity-create-test-file-name "file_name")))
  (should (equal "~/Testfile_name.c"
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

(ert-deftest unity-buffer-is-test-p-test ()
  (save-excursion
    (get-buffer-create  "Testfile.c")
    (set-buffer "Testfile.c")
    (should (equal "Testfile.c"            
                   (buffer-name)))
    (should (unity-buffer-is-test-p))
    
    (get-buffer-create  "file.c")
    (set-buffer "file.c")
    (should (equal "file.c"            
                   (buffer-name)))
    (should-not (unity-buffer-is-test-p))))

(ert-deftest unity-buffer-is-header-p-test ()
  (save-excursion
    (get-buffer-create  "file_name.h")
    (set-buffer "file_name.h")
    (should (equal "file_name.h"            
                   (buffer-name)))
    (should (unity-buffer-is-header-p))
    
    (get-buffer-create  "file_name.c")
    (set-buffer "file_name.c")
    (should (equal "file_name.c"            
                   (buffer-name)))
    (should-not (unity-buffer-is-header-p))))

(ert-deftest unity-find-root-dir-test ()
  (should
   (equal "~/ceedling5/trunk/examples/temp_sensor/"
          (unity-find-root-dir
           "~/ceedling5/trunk/examples/temp_sensor/test/TestAdcConductor.c")))
  (should
   (equal
    "~/ceedling5/trunk/examples/temp_sensor/"
    (unity-find-root-dir
     "~/ceedling5/trunk/examples/temp_sensor/test/AdcConductor.h")))
  (should
   (equal
    "~/ceedling5/trunk/examples/temp_sensor/"
    (unity-find-root-dir
     "~/ceedling5/trunk/examples/temp_sensor/test/AdcConductor.c")))
  (should-not (unity-find-root-dir "/")))

(ert-deftest unity-check-for-ceedling-directories-p-test ()
  (should (unity-check-for-ceedling-directories-p
           "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/"))
  (should-not (unity-check-for-ceedling-directories-p
               "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples"))
  (should-not (unity-check-for-ceedling-directories-p "")))

(ert-deftest unity-search-for-ceedling-root-by-presence-of-relevant-dirs-returns-correct-response ()
  (should (equal
           "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/"
           (unity-search-for-ceedling-root-by-presence-of-relevant-dirs
            "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/")))
  (should (equal
           "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/"
           (unity-search-for-ceedling-root-by-presence-of-relevant-dirs
            "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples")))
  (should-not (equal
               "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/"
               (unity-search-for-ceedling-root-by-presence-of-relevant-dirs
                "~/.emacs.d/martyn/martyn/unity-mode/")))
  (should-not (equal
               "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/"
               (unity-search-for-ceedling-root-by-presence-of-relevant-dirs
                ""))))

(ert-deftest unity-check-for-unity-root-relative-to-ceedling-p-test ()
  (should (unity-check-for-unity-root-relative-to-ceedling-p
           "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/"))
  (should-not (unity-check-for-unity-root-relative-to-ceedling-p
               "~/.emacs.d/martyn/martyn/unity-mode/ceedling/"))
  (should-not (unity-check-for-unity-root-relative-to-ceedling-p
               "")))

(ert-deftest unity-check-for-unity-root-relative-to-ceedling-p-test ()
  (should (unity-check-for-unity-root-relative-to-ceedling-p
           "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/"))
  (should-not (unity-check-for-unity-root-relative-to-ceedling-p
               "~/.emacs.d/martyn/martyn/unity-mode/ceedling/"))
  (should-not (unity-check-for-unity-root-relative-to-ceedling-p
               "")))

(ert-deftest unity-check-for-cmock-root-relative-to-ceedling-p-test ()
  (should (unity-check-for-cmock-root-relative-to-ceedling-p
           "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/"))
  (should-not (unity-check-for-cmock-root-relative-to-ceedling-p
               "~/.emacs.d/martyn/martyn/unity-mode/ceedling/"))
  (should-not (unity-check-for-cmock-root-relative-to-ceedling-p
               "")))

(defun unity-check-for-custom-plugins-relative-to-ceedling-p (directory)
  (file-directory-p (concat directory "custom_plugins/")))

(ert-deftest unity-check-for-custom-plugins-relative-to-ceedling-p-test ()
  (should-not (unity-check-for-custom-plugins-relative-to-ceedling-p
               "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/"))
  (should (unity-check-for-custom-plugins-relative-to-ceedling-p
           "~/ceedling5/trunk/")))

(ert-deftest unity-rakefile-definition ()
  (should (equal "\n\nPROJECT_ROOT  = File.expand_path( File.dirname(__FILE__) )\n
load File.join(File.dirname(__FILE__),'ceedling-rakefile-target.rb')\n
task :default => [:clobber, 'test:all']\n\n" unity-rakefile )))

(ert-deftest unity-rakefile-set-target-generates-correct-result ()
  (should (equal "\n\nPROJECT_ROOT  = File.expand_path( File.dirname(__FILE__) )\n
load File.join(File.dirname(__FILE__),'~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/lib/rakefile.rb')\n
task :default => [:clobber, 'test:all']\n\n"
                 (unity-rakefile-set-target
                 unity-rakefile
                 "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/lib/rakefile.rb")))
  (should (equal "\n\nPROJECT_ROOT  = File.expand_path( File.dirname(__FILE__) )\n
load File.join(File.dirname(__FILE__),'../../lib/rakefile.rb')\n
task :default => [:clobber, 'test:all']\n\n"
                 (unity-rakefile-set-target
                 unity-rakefile
                 "../../lib/rakefile.rb"))))

;; (ert-deftest unity-rakefile-backup-test ()
;;   (should (equal "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/rakefile_Thu_Jan__6_17:46:50_2011.rb"
;;                  (unity-rakefile-backup
;;                   "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/rakefile.rb"
;;                   unity-test-time))))

(ert-deftest unity-generate-backup-name-returns-correct-name ()
  (should (equal "rakefile_Thu_Jan__6_17:46:50_2011.rb"
           (unity-generate-backup-name  "rakefile.rb" unity-test-time)))
  (should-not (unity-generate-backup-name  "rakefile.rbc" unity-test-time)))

(ert-deftest unity-search-for-project-root-by-rakefile-returns-correct-response ()
  (should
   (equal
    "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/"
    (unity-search-for-project-root-by-rakefile
     "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/test/TestAdcConductor.c")))
  (should (equal
           "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/"
           (unity-search-for-project-root-by-rakefile
            "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/")))
  (should (equal
           "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/"
           (unity-search-for-project-root-by-rakefile
            "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples")))
  (should-not (unity-search-for-project-root-by-rakefile "~/")))

;; (ert-deftest unity-search-for-ceedling-root-by-project-yml-returns-correct-response ()
;;   (should
;;    (equal
;;     "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/"
;;     (unity-search-for-project-root-by-rakefile
;;      "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/test/TestAdcConductor.c")))
;;   (should (equal
;;     "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/"
;;     (unity-search-for-project-root-by-rakefile
;;     "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/")))
;;   (should (equal
;;            "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/"
;;            (unity-search-for-project-root-by-rakefile
;;            "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples")))
;;   (should-not (unity-search-for-project-root-by-rakefile "~/")))

;; (ert-deftest unity-find-src-for-test-file-correct-whatever ()
;;   (should (equal ""
;;                  (unity-find-src-for-test-file "test_file.c" "~/ceedling5/trunk/examples/temp_sensor/"))))

;; ;; (ert-deftest unity-find-src-for-test-file-correct-whatever ()
;; ;;   (should (equal ""
;; ;;                  (unity-find-src-for-test-file "test_file.c"))))

(ert-deftest unity-generate-directories-test ()
  (let ((ert-test-dir "~/.emacs.d/martyn/martyn/unity-mode/ert-test/"))
    (if (file-directory-p ert-test-dir)
        (delete-directory ert-test-dir))
    
    (make-directory ert-test-dir)
    (setq unity-directory-list 
          `(,(concat ert-test-dir "test-1/")
            ,(concat ert-test-dir "test-2/")
            ,(concat ert-test-dir "test-3/")))

    (unity-generate-directories unity-directory-list)
    (loop for i in unity-directory-list
          collect(should (file-directory-p i)))
    (loop for i in unity-directory-list
          collect(delete-directory i))

    (delete-directory ert-test-dir)))

(ert-deftest unity-build-missing-directories-list-test ()
  (let ((ert-test-dir "~/.emacs.d/martyn/martyn/unity-mode/ert-test/"))
    (if (file-directory-p ert-test-dir)
        (delete-directory ert-test-dir))

    (unity-generate-directories
     `(,(concat ert-test-dir)
       ,(concat ert-test-dir "test-3/")))

    (should
     (equal
      `(,(concat ert-test-dir "test-2/")
        ,(concat ert-test-dir "test-1/"))
      (unity-build-missing-directories-list
       `(,(concat ert-test-dir "test-1/")
         ,(concat ert-test-dir "test-2/")
         ,(concat ert-test-dir "test-3/")))))

    (should
     (equal
      `(,unity-mocks-dir)
      (unity-build-missing-directories-list
       `(,unity-project-root-dir
         ,unity-ceedling-root-dir
         ,unity-unity-root-dir
         ,unity-ceedling-root-dir
         ,unity-cmock-root-dir
         ,unity-plugins-dir
         ,unity-custom-plugins-dir
         ,unity-src-dir
         ,unity-header-dir
         ,unity-test-dir
         ,unity-mocks-dir
         ,unity-build-dir))))

    (loop for i in 
          `(,(concat ert-test-dir "test-3/"))
          collect(delete-directory i))))

(ert-deftest unity-file-exists-p-test () 
  (should (unity-file-exists-p "TestAdcConductor.c" "test-type"))
  (should (unity-file-exists-p "TestAdcConductor.c" "test-type"))
  (should (unity-file-exists-p "AdcConductor.c" "src-type"))
  (should (unity-file-exists-p "AdcConductor.h" "header-type"))

   (should (file-exists-p "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/src/AdcConductor.c"))

  )
   
(ert-deftest unity-switch-test-src-buffer-creates-correct-filename ()
  (should (equal
     "AdcConductor.c"
    (unity-switch-test-src-buffer
     "TestAdcConductor.c" t))))

(ert-deftest unity-switch-src-header-buffer-creates-correct-filename ()
  (should (equal
     "AdcConductor.h"
    (unity-switch-src-header-buffer "AdcConductor.c" t))))

(ert-deftest unity-switch-test-header-buffer-creates-correct-filename ()
  (should (equal
     "AdcConductor.h"
    (unity-switch-test-header-buffer
     "AdcConductor.c" t))))

(ert-deftest unity-switch-src-test-buffer-creates-correct-filename ()
  (should (equal
     "TestAdcConductor.c"
    (unity-switch-src-test-buffer
     "AdcConductor.c" t))))

(ert-deftest unity-switch-header-test-buffer-creates-correct-filename ()
  (should (equal
     "TestAdcConductor.c"
    (unity-switch-header-test-buffer
     "AdcConductor.h" t))))

(ert-deftest unity-switch-header-src-buffer-creates-correct-filename ()
  (should (equal
     "AdcConductor.c"
    (unity-switch-header-src-buffer
     "AdcConductor.h" t)))
  (should (equal
    "AdcHardwareConfigurator.c"
    (unity-switch-header-src-buffer
     "AdcHardwareConfigurator.h" t))))

(ert-deftest unity-create-src-file-name-test ()
  (should (equal "AdcConductor.c"
    (unity-create-src-file-name "TestAdcConductor.c")))
  (should-not (equal "AdcConductor"
    (unity-create-src-file-name "testAdcConductor"))))
    
