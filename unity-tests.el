
(provide 'unity-tests)

(setq unity-tests-root  "~/.emacs.d/martyn/martyn/unity-mode/")
(setq unity-test-project-root
      (concat unity-tests-root
              "ceedling/trunk/examples/temp_sensor/"))
(setq unity-test-time '(19750 . 10)) ;;used for ert tests

(ert-deftest unity-check-prefix-test-p ()
  (should (unity-check-prefix-p
           "Testsomething.c" "test-file"))
  (should (unity-check-prefix-p
           "TestsomethingConductor.c" "test-file"))
  (should-not (unity-check-prefix-p
               "something.h" "test-file"))
  (should (unity-check-prefix-p
           "Testsomething.h" "test-file"))
  (should (unity-check-prefix-p
           "testsomething.h" "src-file"))
  (should (unity-check-prefix-p
           "Test_fileConductor.c" "test-file"))
  (should (unity-check-prefix-p
           "fileConductor.c" "src-file"))
  )

(ert-deftest unit-check-suffix-p-test ()
  (unity-check-suffix-p
   "somethingConductor.c" "conductor-file")
  (unity-check-suffix-p
   "somethingHardware.c" "hardware-file")
  (unity-check-suffix-p
   "somethingModel.c" "model-file")
  )

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
  (should-not (unity-is-test-file-p "~/docs/Test_file.ccc"))
  )

(ert-deftest unity-check-extension-p-test ()
  (should  (unity-check-extension-p "name.h" "header-file"))
  (should  (unity-check-extension-p "name.c""src-file"))
  (should  (unity-check-extension-p "name.c" "test-file"))
  (should  (unity-check-extension-p "name.c" "conductor-file"))
  (should  (unity-check-extension-p "name.c" "hardware-file"))
  (should  (unity-check-extension-p "name.c" "model-file"))
  )

(ert-deftest unity-is-file-type-p-tests ()
  (should (unity-is-file-type-p "Test_file.c" "test-file"))
  (should-not (unity-is-file-type-p "Test_file.h" "test-file"))
  (should-not (unity-is-file-type-p "file.c" "test-file"))
  (should (unity-is-file-type-p "~/test/Test_file.c" "test-file"))
  (should-not (unity-is-file-type-p "~/docs/Test_file.ccc" "test-file"))
  (should-not (unity-is-file-type-p "" "test-file"))

  (should (unity-is-file-type-p "Test_file.c" "src-file")) ;TODO!!
  (should-not (unity-is-file-type-p "file.h" "src-file"))
  (should (unity-is-file-type-p "file.c" "src-file"))
  (should (unity-is-file-type-p "~/src/src/file.c" "src-file"))
  (should-not (unity-is-file-type-p "~/build/file.cout" "src-file"))
  (should-not (unity-is-file-type-p "file" "src-file"))
  (should-not (unity-is-file-type-p "" "src-file"))
  )

(ert-deftest unity-is-src-file-p-returns-correct-result ()
  (should (unity-is-src-file-p "Test_file.c")) ;TODO is this what we want?
  (should (unity-is-src-file-p "fileConductor.c"))
  (should-not (unity-is-src-file-p "file.h")) 
  (should (unity-is-src-file-p "file.c"))
  (should (unity-is-src-file-p "~/src/src/file.c"))
  (should-not (unity-is-src-file-p "~/build/file.cout"))
  )

(ert-deftest unity-is-header-file-p-returns-correct-result ()
  (should (unity-is-header-file-p "Test_file.h")) ;;TODO is this
  ;;what we want?
  (should-not (unity-is-header-file-p "file.c"))
  (should (unity-is-header-file-p "file.h"))
  (should (unity-is-header-file-p "~/inc/file.h"))
  (should-not (unity-is-header-file-p "~/docs/file.html")))

(ert-deftest unity-is-ruby-file-p-returns-correct-result ()
  (should (unity-is-ruby-file-p "file.rb"))
  (should-not (unity-is-ruby-file-p "file.c"))
  (should-not (unity-is-ruby-file-p "file"))
  (should (unity-is-ruby-file-p "~/inc/file.rb"))
  (should-not (unity-is-ruby-file-p "~/docs/file.rbcd")))

(ert-deftest unity-is-code-file-p-returns-expected ()
  (should (unity-is-code-file-p  "~/test/Test_file.c"))
  (should (unity-is-code-file-p  "~/src/src_file.c"))
  (should (unity-is-code-file-p  "~/inc/header_file.c"))
  (should-not (unity-is-code-file-p  "~/docs/some_file.html")))


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
                 (unity-create-src-file-name "Testfile_name.c")))
  (should (equal "file_name.c"
                 (unity-create-src-file-name "Testfile_name")))
  (should (equal "~/file_name.c"
                 (unity-create-src-file-name "~/Testfile_name.c"))))

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

;; ;; (ert-deftest unity-find-root-dir-test ()
;; ;;   (should
;; ;;    (equal "~/ceedling5/trunk/examples/temp_sensor/"
;; ;;           (unity-find-root-dir
;; ;;            "~/ceedling5/trunk/examples/temp_sensor/test/TestAdcConductor.c")))
;; ;;   (should
;; ;;    (equal
;; ;;     "~/ceedling5/trunk/examples/temp_sensor/"
;; ;;     (unity-find-root-dir
;; ;;      "~/ceedling5/trunk/examples/temp_sensor/test/AdcConductor.h")))
;; ;;   (should
;; ;;    (equal
;; ;;     "~/ceedling5/trunk/examples/temp_sensor/"
;; ;;     (unity-find-root-dir
;; ;;      "~/ceedling5/trunk/examples/temp_sensor/test/AdcConductor.c")))
;; ;;   (should-not (unity-find-root-dir "/")))

(ert-deftest unity-check-for-ceedling-directories-p-test ()
  (should (unity-check-for-ceedling-directories-p
           (concat unity-tests-root "ceedling/trunk/")))
  (should-not (unity-check-for-ceedling-directories-p
               (concat unity-tests-root "ceedling/trunk/examples")))
  (should-not (unity-check-for-ceedling-directories-p "")))

(ert-deftest unity-search-for-ceedling-root-by-presence-of-relevant-dirs-returns-correct-response ()
  (should (equal
           (concat unity-tests-root "ceedling/trunk/")
           (unity-search-for-ceedling-root-by-presence-of-relevant-dirs
            (concat unity-tests-root "ceedling/trunk/"))))
  (should (equal
           (concat unity-tests-root "ceedling/trunk/")
           (unity-search-for-ceedling-root-by-presence-of-relevant-dirs
            (concat unity-tests-root "ceedling/trunk/examples"))))
  (should-not (equal
               (concat unity-tests-root "ceedling/trunk/")
               (unity-search-for-ceedling-root-by-presence-of-relevant-dirs
                unity-tests-root )))
  (should-not (equal
               (concat unity-tests-root "ceedling/trunk/")
               (unity-search-for-ceedling-root-by-presence-of-relevant-dirs
                ""))))

(ert-deftest unity-check-for-unity-root-relative-to-ceedling-p-test ()
  (should (unity-check-for-unity-root-relative-to-ceedling-p
           (concat unity-tests-root "ceedling/trunk/")))
  (should-not (unity-check-for-unity-root-relative-to-ceedling-p
               (concat unity-tests-root "ceedling/")))
  (should-not (unity-check-for-unity-root-relative-to-ceedling-p
               "")))

(ert-deftest unity-check-for-unity-root-relative-to-ceedling-p-test ()
  (should (unity-check-for-unity-root-relative-to-ceedling-p
           (concat unity-tests-root "ceedling/trunk/")))
  (should-not (unity-check-for-unity-root-relative-to-ceedling-p
               (concat unity-tests-root "ceedling/")))
  (should-not (unity-check-for-unity-root-relative-to-ceedling-p
               "")))

(ert-deftest unity-check-for-cmock-root-relative-to-ceedling-p-test ()
  (should (unity-check-for-cmock-root-relative-to-ceedling-p
           (concat unity-tests-root "ceedling/trunk/")))
  (should-not (unity-check-for-cmock-root-relative-to-ceedling-p
               (concat unity-tests-root "ceedling/")))
  (should-not (unity-check-for-cmock-root-relative-to-ceedling-p
               "")))

(defun unity-check-for-custom-plugins-relative-to-ceedling-p (directory)
  (file-directory-p (concat directory "custom_plugins/")))

(ert-deftest unity-check-for-custom-plugins-relative-to-ceedling-p-test ()
  (should-not (unity-check-for-custom-plugins-relative-to-ceedling-p
               (concat unity-tests-root "ceedling/trunk/")))
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
    unity-test-project-root
    (unity-search-for-project-root-by-rakefile
     (concat unity-test-project-root "test/TestAdcConductor.c"))))
  (should
   (equal
    unity-test-project-root
    (unity-search-for-project-root-by-rakefile
     unity-test-project-root
     )))
  (should
   (equal
    (concat unity-tests-root "ceedling/trunk/")
    (unity-search-for-project-root-by-rakefile
     (concat unity-tests-root "ceedling/trunk/examples"))))
  (should-not
   (unity-search-for-project-root-by-rakefile "~/")))

;; ;; ;; (ert-deftest unity-find-src-for-test-file-correct-whatever ()
;; ;; ;;   (should (equal ""
;; ;; ;;                  (unity-find-src-for-test-file "test_file.c" "~/ceedling5/trunk/examples/temp_sensor/"))))

;; ;; ;; (ert-deftest unity-find-src-for-test-file-correct-whatever ()
;; ;; ;;   (should (equal ""
;; ;; ;;                  (unity-find-src-for-test-file "test_file.c"))))

;; ;; ;; (ert-deftest unity-generate-directories-test ()
;; ;; ;;   (let ((ert-test-dir "~/.emacs.d/martyn/martyn/unity-mode/ert-test/"))
;; ;; ;;     (if (file-directory-p ert-test-dir)
;; ;; ;;         (delete-directory ert-test-dir))

;; ;; ;;     (make-directory ert-test-dir)
;; ;; ;;     (setq unity-directory-list 
;; ;; ;;           `(,(concat ert-test-dir "test-1/")
;; ;; ;;             ,(concat ert-test-dir "test-2/")
;; ;; ;;             ,(concat ert-test-dir "test-3/")))

;; ;; ;;     (unity-generate-directories unity-directory-list)
;; ;; ;;     (loop for i in unity-directory-list
;; ;; ;;           collect(should (file-directory-p i)))
;; ;; ;;     (loop for i in unity-directory-list
;; ;; ;;           collect(delete-directory i))

;; ;; ;;     (delete-directory ert-test-dir)))

;; ;; ;; (ert-deftest unity-build-missing-directories-list-test ()
;; ;; ;;   (let ((ert-test-dir "~/.emacs.d/martyn/martyn/unity-mode/ert-test/"))
;; ;; ;;     (if (file-directory-p ert-test-dir)
;; ;; ;;         (delete-directory ert-test-dir))

;; ;; ;;     (unity-generate-directories
;; ;; ;;      `(,(concat ert-test-dir)
;; ;; ;;        ,(concat ert-test-dir "test-3/")))

;; ;; ;;     (should
;; ;; ;;      (equal
;; ;; ;;       `(,(concat ert-test-dir "test-2/")
;; ;; ;;         ,(concat ert-test-dir "test-1/"))
;; ;; ;;       (unity-build-missing-directories-list
;; ;; ;;        `(,(concat ert-test-dir "test-1/")
;; ;; ;;          ,(concat ert-test-dir "test-2/")
;; ;; ;;          ,(concat ert-test-dir "test-3/")))))

;; ;; ;;     (should
;; ;; ;;      (equal
;; ;; ;;       `(,unity-mocks-dir)
;; ;; ;;       (unity-build-missing-directories-list
;; ;; ;;        `(,unity-project-root-dir
;; ;; ;;          ,unity-ceedling-root-dir
;; ;; ;;          ,unity-unity-root-dir
;; ;; ;;          ,unity-ceedling-root-dir
;; ;; ;;          ,unity-cmock-root-dir
;; ;; ;;          ,unity-plugins-dir
;; ;; ;;          ,unity-custom-plugins-dir
;; ;; ;;          ,unity-src-dir
;; ;; ;;          ,unity-header-dir
;; ;; ;;          ,unity-test-dir
;; ;; ;;          ,unity-mocks-dir
;; ;; ;;          ,unity-build-dir))))

;; ;; ;;     (loop for i in 
;; ;; ;;           `(,(concat ert-test-dir "test-3/"))
;; ;; ;;           collect(delete-directory i))))

;; ;; ;;(ert-deftest unity-file-exists-p-test () 
;; ;; ;; (should (unity-file-exists-p "TestAdcConductor.c" "test-type"))
;; ;; ;; (should (unity-file-exists-p "TestAdcConductor.c" "test-type"))
;; ;; ;; (should (unity-file-exists-p "AdcConductor.c" "src-type"))
;; ;; ;; (should (unity-file-exists-p "AdcConductor.h" "header-type"))

;; ;; ;; (should
;; ;; ;;  (unity-file-exists-p
;; ;; ;;   "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/src/AdcConductor.c")))


(ert-deftest unity-switch-buffer-with-switch-type-test-to-src ()
  (should (equal "AdcConductor.c"
                 (unity-switch-buffer
                  "TestAdcConductor.c" "test-to-src" "non-mch-type" t))))

(ert-deftest unity-switch-buffer-with-switch-type-test-to-src ()
  (should (equal "AdcConductor.h"
                 (unity-switch-buffer
                  "AdcConductor.c" "src-to-header" "non-mch-type" t))))

(ert-deftest unity-switch-buffer-with-switch-type-test-to-header ()
  (should (equal "AdcConductor.h"
                 (unity-switch-buffer
                  "TestAdcConductor.c" "test-to-header" "non-mch-type" t))))

(ert-deftest unity-switch-buffer-with-switch-type-src-to-test ()
  (should (equal "TestAdcConductor.c"
                 (unity-switch-buffer
                  "AdcConductor.c" "src-to-test" "non-mch-type" t))))

(ert-deftest unity-switch-buffer-with-switch-type-header-to-test ()
  (should (equal "TestAdcConductor.c"
                 (unity-switch-buffer
                  "AdcConductor.h" "header-to-test" "non-mch-type" t))))

(ert-deftest unity-switch-buffer-with-switch-type-header-to-src ()
  (should (equal "AdcHardwareConfigurator.c"
                 (unity-switch-buffer
                  "AdcHardwareConfigurator.h" "header-to-src" "non-mch-type" t))))

(ert-deftest unity-switch-buffer-with-switch-type-model-to-conductor ()
  (should (equal "AdcConductor.c"
                 (unity-switch-buffer
                  "AdcModel.c" "model-to-conductor" "mch-type" t)))
  )
(ert-deftest unity-switch-buffer-with-switch-type-model-to-hardware ()
  (should (equal "AdcHardware.c"
                 (unity-switch-buffer
                  "AdcModel.c" "model-to-hardware" "mch-type" t))))

(ert-deftest unity-switch-buffer-with-switch-type-conductor-to-hardware ()
  (should (equal "AdcHardware.c"
                 (unity-switch-buffer
                  "AdcConductor.c" "conductor-to-hardware" "mch-type" t))))

(ert-deftest unity-switch-buffer-with-switch-type-conductor-to-model ()
  (should (equal "AdcModel.c"
                 (unity-switch-buffer
                  "AdcConductor.c" "conductor-to-model" "mch-type" t))))

(ert-deftest unity-switch-buffer-with-switch-type-hardware-to-model ()
  (should (equal "AdcModel.c"
                 (unity-switch-buffer
                  "AdcHardware.c" "hardware-to-model" "mch-type" t))))

(ert-deftest unity-switch-buffer-with-switch-type-hardware-to-conductor ()
  (should (equal "AdcConductor.c"
                 (unity-switch-buffer
                  "AdcHardware.c" "hardware-to-conductor" "mch-type" t))))

(ert-deftest unity-switch-buffer-with-test-file-type-and-switch-type-hardware-to-conductor ()
  (should (equal "TestAdcConductor.c"
                 (unity-switch-buffer
                  "TestAdcHardware.c" "hardware-to-conductor" "mch-type" t))))

(ert-deftest unity-switch-buffer-with-header-and-switch-type-hardware-to-conductor ()
  (should (equal "AdcConductor.c"
                 (unity-switch-buffer
                  "AdcHardware.h" "hardware-to-conductor" "mch-type" t))))

(ert-deftest unity-create-src-file-name-test ()
  (should (equal "AdcConductor.c"
                 (unity-create-src-file-name "TestAdcConductor.c")))
  (should-not (equal "AdcConductor"
                     (unity-create-src-file-name "testAdcConductor"))))


(ert-deftest unity-convert-file-name-with-conductor-to-model-and-mch-type ()
  (should
   (equal
    "nameModel.c"
    (unity-convert-file-name
     "nameConductor.h"
     "conductor-to-model" "mch-type"))))

(ert-deftest unity-convert-file-name-with-conductor-to-model-and-mch-type-with-path ()
  (should
   (equal
    "~/nameModel.c"
    (unity-convert-file-name
     "~/nameConductor.h"
     "conductor-to-model" "mch-type"))))

(ert-deftest unity-convert-file-name-with-conductor-to-hardware-and-mch-type ()
  (should
   (equal
    "nameHardware.c"
    (unity-convert-file-name
     "nameConductor.c"
     "conductor-to-hardware" "mch-type"))))

(ert-deftest unity-convert-file-name-with-model-to-conductor-and-mch-type ()
  (should
   (equal
    "nameConductor.c"
    (unity-convert-file-name
     "nameModel.h"
     "model-to-conductor" "mch-type"))))

(ert-deftest unity-convert-file-name-with-model-to-hardware-and-mch-type ()
  (should
   (equal
    "~/nameHardware.c"
    (unity-convert-file-name
     "~/nameModel.h"
     "model-to-hardware" "mch-type"))))

(ert-deftest unity-convert-file-name-with-hardware-to-model-and-mch-type ()
  (should
   (equal
    "nameModel.c"
    (unity-convert-file-name
     "nameHardware.c"
     "hardware-to-model" "mch-type"))))

(ert-deftest unity-convert-file-name-with-src-to-header-and-non-mch-type ()
  (should
   (equal
    "name.h"
    (unity-convert-file-name
     "name.c"
     "src-to-header" "non-mch-type"))))

(ert-deftest unity-convert-file-name-with-src-to-test-and-non-mch-type ()
  (should
   (equal
    "Testname.c"
    (unity-convert-file-name
     "name.c"
     "src-to-test" "non-mch-type"))))

(ert-deftest unity-convert-file-name-with-header-to-test-and-non-mch-type ()
  (should
   (equal
    "Testname.c"
    (unity-convert-file-name
     "name.h"
     "header-to-test" "non-mch-type"))))

(ert-deftest unity-convert-file-name-with-header-to-src-and-non-mch-type ()
  (should
   (equal
    "name.c"
    (unity-convert-file-name
     "name.h"
     "header-to-src" "non-mch-type"))))

(ert-deftest unity-convert-file-name-with-test-to-src-and-non-mch-type ()
  (should
   (equal
    "name.c"
    (unity-convert-file-name
     "Testname.c"
     "test-to-src" "non-mch-type"))))

(ert-deftest unity-convert-file-name-with-test-to-header-and-non-mch-type ()
  (should
   (equal
    "name.h"
    (unity-convert-file-name
     "Testname.c"
     "test-to-header" "non-mch-type"))))

(ert-deftest unity-is-model-file-p-test ()
  (should (unity-is-model-file-p "AdcModel.c"))
  (should-not (unity-is-conductor-file-p "Adc.c"))
  (should-not (unity-is-conductor-file-p "Adc_conductor.c")))


(ert-deftest unity-is-conductor-file-p-test ()
  (should (unity-is-conductor-file-p "AdcConductor.c"))
  (should-not (unity-is-conductor-file-p "Adc.c"))
  (should-not (unity-is-conductor-file-p "Adc_conductor.c")))

(ert-deftest unity-is-hardware-file-p-test ()
  (should (unity-is-hardware-file-p "AdcHardware.c"))
  (should-not (unity-is-hardware-file-p "Adchardware.c"))
  (should-not (unity-is-hardware-file-p "AdcModel.c"))
  (should-not (unity-is-hardware-file-p "Adchardware.h")) ;TODO is this what we want?
  (should (unity-is-hardware-file-p "nameHardware.c")))

(ert-deftest unity-error-test ()
  (should (equal "Error! (nil error message) !"
                 (unity-error nil t)))
  (should (equal "Error! Invalid something in file-name!"
                 (unity-error "Invalid something in file-name" t))))

(ert-deftest unity-error-with-param-test ()
  (should (equal "Error! Invalid type name (file-type) in file-name!"
                 (unity-error-with-param
                  "Invalid type name"
                  "file-type"
                  "in file-name"
                  t)))
  (should (equal "Error! Invalid type name (nil) in file-name!"
                 (unity-error-with-param
                  "Invalid type name"
                  nil
                  "in file-name"
                  t)))
  (should (equal "Error!  (file-type) in file-name!"
                 (unity-error-with-param
                  nil
                  "file-type"
                  "in file-name"
                  t)))
  (should (equal "Error! Invalid type name (file-type) !"
                 (unity-error-with-param
                  "Invalid type name"
                  "file-type"
                  nil
                  t))))

(ert-deftest unity-file-prefix-test ()
  (should (equal "Test" (unity-file-prefix "test-file")))
  (should (equal "" (unity-file-prefix "src-file")))
  (should (equal "" (unity-file-prefix "header-file")))
  (should (equal "" (unity-file-prefix "model-file")))
  (should (equal "" (unity-file-prefix "conductor-file")))
  (should (equal "" (unity-file-prefix "hardware-file"))))

(ert-deftest unity-file-prefix-list-test ()
  (should (equal '("Test") (unity-file-prefix-list)))
  (should-not (equal '("test") (unity-file-prefix-list))))

(ert-deftest unity-file-suffix-list-test ()
  (should (equal '("Model" "Conductor" "Hardware") (unity-file-suffix-list))))

(ert-deftest unity-strip-affix-test ()
  (should (equal "name.c" (unity-strip-affix "Testname.c" "prefix-type")))
  (should (equal "Testname.c" (unity-strip-affix "Testname.c" "suffix-type")))
  (should (equal "Testname.c" (unity-strip-affix "Testname.c" "suffix-type")))
  (should (equal "name.c" (unity-strip-affix "Testname.c" "prefix-type")))
  )

;; (should (equal "" (unity-read-attribute "name" "header-type")))
;; (should (equal ".h" (unity-read-attribute "name.h" "header-type")))
;; (should(not(equal nil (unity-read-attribute "" "header-type"))))


                                        ;  (should-not (equal '("test") (unity-file-prefix-list))))

(ert-deftest unity-file-extension-test ()
  (should (equal ".c" (unity-file-extension "test-file")))
  (should (equal ".c" (unity-file-extension "src-file")))
  (should (equal ".h" (unity-file-extension "header-file"))))

(ert-deftest unity-read-name-test ()
  (should (equal "name" (unity-read-name "Testname.c")))
  (should (equal "testname" (unity-read-name "testname.h"))))

(ert-deftest unity-original-replace-regexp-in-string-tests ()
  ;;FAILS...
  (should-not(equal "a" (replace-regexp-in-string "A" "B" "a" nil nil nil nil)))
  ;;FAILS...
  (should-not(equal "A" (replace-regexp-in-string "a" "B" "A" t t nil nil)))
  ;;FAILS...
  (should-not(equal "a" (replace-regexp-in-string "A" "B" "a" nil t nil nil)))
  ;;FAILS...
  (should-not(equal "A" (replace-regexp-in-string "a" "B" "A" nil t nil nil)))
  ;;FAILS...
  (should-not (equal "a" (replace-regexp-in-string "A" "a" "A" nil t nil nil)))
  ;;FAILS
  (should-not (equal "a" (replace-regexp-in-string "A" "a" "A" nil nil nil nil)))
  (should
   (equal "aaaaa" (replace-regexp-in-string "A" "a" "AaAaA" t t nil nil)))
  (should
   (equal "AAAAA" (replace-regexp-in-string "a" "A" "AaAaA" t t nil nil)))
  ;;FAILS
  (should-not
   (equal "aaaaa" (replace-regexp-in-string "A" "a" "AaAaA" nil nil nil nil)))
  (should
   (equal "AAAAA" (replace-regexp-in-string "a" "A" "AaAaA" nil nil nil nil)))

  )

(ert-deftest unity-replace-regex-in-string-tests ()
  (should(equal "a" (unity-replace-regex-in-string "A" "B" "a")))
  ;;  (should(equal "A" (unity-replace-regex-in-string "a" "B" "A"))
  (should(equal "A" (unity-replace-regex-in-string "a" "A" "a")))
  (should(equal "a" (unity-replace-regex-in-string "A" "a" "A")))
  (should
   (equal "aaaaa" (unity-replace-regex-in-string "A" "a" "AaAaA")))
  (should
   (equal "AAAAA" (unity-replace-regex-in-string "a" "A" "AaAaA")))
  )

(ert-deftest unity-string-exact-match-tests ()
  (should (unity-string-exact-match "a" "a"))
  (should-not (unity-string-exact-match "a" "Ba"))
  (should-not (unity-string-exact-match "a" "Ba"))
  (should-not (unity-string-exact-match "c" "abc"))
  (should-not (unity-string-exact-match "a" "A"))
  (should-not (unity-string-exact-match "5" "1"))
  (should (unity-string-exact-match "" ""))
  (should-not (unity-string-exact-match "" "a")) ;; !! ** watch this
  ;; with standard string-match!!
  (should-not (unity-string-exact-match "A" ""))
  (should (unity-string-exact-match "12345" "12345")))

(ert-deftest unity-read-extension-test ()
  (should (equal ".c" (unity-read-attribute "name.c" "extension-type")))
  (should (equal ""   (unity-read-attribute "name" "extension-type")))
  (should (equal ""   (unity-read-attribute "name." "extension-type")))
  (should (equal ""   (unity-read-attribute "" "extension-type")))
  )
(ert-deftest unity-remove-extension-test ()
  (should (equal "name" (unity-remove-extension "name.c")))
  (should (equal "name"   (unity-remove-extension "name")))
  (should (equal "name."   (unity-remove-extension "name.")))
  (should (equal ""   (unity-remove-extension "")))
  )
(ert-deftest unity-read-attribute-test ()
  (should (equal ".c" (unity-read-attribute "name.c" "extension-type")))
  (should (equal ""   (unity-read-attribute "name" "extension-type")))
  (should (equal ""   (unity-read-attribute "name." "extension-type")))
  (should (equal ""   (unity-read-attribute "" "extension-type")))
  )

(ert-deftest unity-operate-sub-extension-test ()
  (should (equal "Filename.h"
                 (operate-sub-extension 'unity-strip-affix-core
                                        "FilenameConductor.h"
                                        "suffix-type")))
  (should (equal "Filename.h"
                 (operate-sub-extension 'unity-strip-affix-core
                                        "FilenameModel.h"
                                        "suffix-type")))
  (should (equal "Filename.h"
                 (operate-sub-extension 'unity-strip-affix-core
                                        "FilenameHardware.h"
                                        "suffix-type")))
  (should (equal "Filename.h"
                 (operate-sub-extension 'unity-strip-affix-core
                                        "Filename.h"
                                        "suffix-type")))
  (should (equal "Filename."
                 (operate-sub-extension 'unity-strip-affix-core
                                        "Filename."
                                        "suffix-type")))
  (should (equal "Filename.hhh"
                 (operate-sub-extension 'unity-strip-affix-core
                                        "FilenameConductor.hhh"
                                        "suffix-type"))))

(ert-deftest unity-read-prefix-test ()
  (should (equal "Test" (unity-read-prefix "TestSomeNameConductor.c")))
  )

(ert-deftest unity-cycle-test-src-header-buffer-test ()
  (should(equal
          "AdcConductor.c"
          (unity-cycle-test-src-header-buffer
           t
           "TestAdcConductor.c" )))
  (should(equal
          "AdcConductor.h"
          (unity-cycle-test-src-header-buffer
           t
           "AdcConductor.c" )))
  (should(equal
          "TestAdcConductor.c"
          (unity-cycle-test-src-header-buffer
           t
           "AdcConductor.h" )))
  )
 (ert-deftest unity-cycle-MCH-buffer-test ()
  (should(equal
          "AdcConductor.c"
          (unity-cycle-MCH-buffer
           t
           "AdcModel.c" )))
  (should(equal
          "TestAdcConductor.c"
          (unity-cycle-MCH-buffer
           t
           "TestAdcModel.c" )))
  (should(equal
          "AdcHardware.c"
          (unity-cycle-MCH-buffer
           t
           "AdcConductor.c" )))
  (should(equal
          "AdcModel.c"
          (unity-cycle-MCH-buffer
           t
           "AdcHardware.c" )))
 )

(ert-deftest unity-cycle-alphabetic-group ()
  (should(equal
          "AdcConductor.c"
          (unity-cycle-test-src-header-buffer
           t
           "TestAdcConductor.c" )))
  )

(ert-deftest unity-buffer-file-name-test ()
  (should-not (buffer-file-name))) ;NOTE! The testing scope messes this up

(ert-deftest unity-index-current-buffer-test ()
  (should (equal
           4
           (unity-index-current-buffer
            (concat unity-test-project-root
                    "src/AdcConductor.c") t)))
  (should (equal
           6
           (unity-index-current-buffer
            (concat unity-test-project-root "src/AdcHardware.c") t)))
  )

(ert-deftest unity-read-suffix-test ()
  (should (equal
           "Conductor"
           (unity-read-suffix "AdcConductor.c")))
  (should (equal
           "Hardware"
           (unity-read-suffix "AdcHardware.c")))
  ;; (should (equal
  ;;          "Model"
  ;;          (unity-read-suffix "AdcModel.c")))
  ;; (should (equal
  ;;          ""
  ;;          (unity-read-suffix "Adc.h")))
  ;; (should (equal
  ;;          ""
  ;;          (unity-read-suffix "Adc.c")))
  ;;  (should (equal
  ;;           ""
  ;;           (unity-read-suffix "TestAdc.c")))
  )

(ert-deftest unity-get-suffix-file-type ()
  (should (equal "controller-file" (unity-get-suffix-file-type "Controller")))
  (should (equal "model-file" (unity-get-suffix-file-type "Model")))
  (should (equal "hardware-file" (unity-get-suffix-file-type "Hardware")))
  (should (equal "non-pattern-file" (unity-get-suffix-file-type "")))
  )

(ert-deftest unity-search-buffer-test ()
  (should
   (equal
    "AdcHardware.c"
    (unity-search-buffer
     4
     (concat unity-test-project-root
             "src/AdcConductor.c")
     "higher" "ascending" t)))
  (should
   (equal
    "AdcHardwareConfigurator.c"
    (unity-search-buffer
     6
     (concat unity-test-project-root
             "src/AdcHardware.c")
     "higher" "ascending" t)))
  (should
   (equal
    "UsartTransmitBufferStatus.c"
    (unity-search-buffer
     6
     (concat unity-test-project-root
             "src/AdcConductor.c")
     "higher" "descending" t)))
  )

(defun unity-file-suffix1-list ()
  `(,unity-model-file-suffix
    ,unity-conductor-file-suffix
    ,unity-hardware-file-suffix))

;; (ert-deftest unity-test ()
;;   (
;;    '("123" . 123)
;;    (  `(,unity-model-file-suffix
;;         ,unity-conductor-file-suffix
;;         ,unity-hardware-file-suffix))))

;; (ert-deftest unity-cycle-convert-file-name-test()
;;   (should (equal
;;            (concat unity-test-project-root "src/nameConductor.c")
;;                  (unity-cycle-convert-file-name
;;                   (concat
;;                    unity-test-project-root
;;                    "nameModel.c")
;;                   "model-to-conductor"
;;                   "mch-type" t)))
;;   )

(ert-deftest unity-switch-mch-test ()
  (should (unity-switch-mch "AdcConductor.c" "mch-type" t))
  (should (unity-switch-mch "AdcHardware.c" "mch-type" t))
  (should (unity-switch-mch "AdcModel.c" "mch-type" t))
  (should (unity-switch-mch "AdcModel.h" "non-mch-type" t))
  )
