(setq unity-project-root "/home/martyn/ceedling2/trunk/vendor/cmock/examples/")

(defun unity-write-project-file (&optional project-name)
  "Parse project org file and generate yml config file."
  (interactive)
  (if (not (unity-parse-project))
      (error "ERROR! Failed to parse project.org")
    (with-temp-buffer 
      (goto-char (point-min))
      (unity-insert-yml-header)
      (setq unity-depths (reverse unity-depths))
      (setq unity-commands (reverse unity-commands))
      (while unity-depths
        (progn
          (let ((current-depth (car unity-depths)) (current-command (car unity-commands)))
            (setq unity-depths (cdr unity-depths))
            (setq unity-commands (cdr unity-commands))
            (if (and unity-depths current-depth)
                (if (< current-depth (car unity-depths))
                    (progn
                      (insert (unity-heading current-command current-depth))
                      (unity-insert-return))
                  (progn
                    (insert (unity-parameter current-command current-depth))
                    (unity-insert-return)))))))
      
      (unity-insert-yml-footer)
      (when (file-writable-p unity-project-root)
        (write-region (point-min)
                      (point-max)
                      (concat unity-project-root "project.yml"))))))

(defun unity-parse-project ()
  "Parse contents of project.org"
  (interactive)
  (setq unity-depths '())
  (setq unity-commands '())
  (if (file-readable-p (concat unity-project-root "org/project.org"))
    (let ((buffer (find-file-noselect (concat unity-project-root "org/project.org"))))
      (set-buffer buffer)
      (goto-char (point-min))
      (while (and (not (org-current-level)) (not (eobp)))
        (next-line))
      (outline-next-heading) ;;ignore first heading
      (while (not (eobp))
        (setq unity-depths (cons (org-current-level) unity-depths))
        (setq unity-commands (cons (unity-read-command)  unity-commands))
        (outline-next-heading))
      t)
    nil))

(defun unity-process-lower-node (str)
  (unity-replace-command-defaults
   (unity-replace-space-underscore
    (unity-strip-whitespace
     (unity-remove-stars
      str)))))

(defun unity-read-command ()
  (beginning-of-line)
  (let ((start (point)))
    (end-of-line)
    (unity-strip-whitespace
     (unity-remove-stars
      (buffer-substring start (point))))))

(defun unity-remove-stars (heading)
  (message "%s" (unity-replace-regex-in-string "^\**" "" heading)))

(defun unity-insert-return ()
  (insert "\n"))

(defun unity-insert-space ()
  (insert " "))

(defun unity-insert-yml-header ()
  "Inserts default project.yml header."
    (insert "...\n")
    (insert " \n")
    (insert "##           THIS FILE WAS AUTO-GENERATED          ## \n")
    (insert "##         on ")   
    (insert (format-time-string "%c         ##\n" (current-time)))
    (insert "##                  Do not modify!                 ## \n")
    (insert "##             To adjust these settings            ## \n")
    (insert "##                Modify project.org               ## \n")
    (insert "##                   and execute                   ## \n")
    (insert "##            unity-write-project-file()           ## \n")
    (insert "##                  (  C-c,wp )                    ## \n")
    (insert "\n"))

(defun unity-insert-yml-footer ()
  (insert "...\n"))
  
(defun unity-replace-command-defaults (cmd)
  (setq unity-lower-nodes-default-command-list 
        '(
          "project"
          "build_root"
          "use_exceptions"
          "use_mocks"
          "use_test_preprocessor"
          "use_auxiliary_dependencies"
          "test_file_prefix"
          "options_path"
          "release_build"

          "release_build"
          "use_assembly"

          "paths"
          "test"
          "source"
          "support"
          "include"
          "test_toolchain_include"
          "release_toolchain_include"

          "environment"
          "rake_columns"

          "defines"
          "test"
          "test_preprocess"
          "release"
          "release_preprocess"
          
          "extension"
          "header"
          "source"
          "assembly"
          "object"
          "executable"
          "testpass"
          "testfail"
          "dependencies"

          "unity"
          "defines"

          "cmock"
          "defines"
          "unity_helper"
          
          "cexception"
          "defines"

          "test_runner"
          "includes"
          "file_suffix"

          "tools"
          "test_compiler"
          "test_linker"
          "test_fixture"
          "test_includes_preprocessor"
          "test_file_preprocessor"
          "test_dependencies_generator"
          "release_compiler"
          "release_assembler"
          "release_linker"
          "release_dependencies_generator"
          "arguments"
          
          "plugins"
          "base_path"
          "auxiliary_load_path"
          "enabled"
          ))

  (catch 'break
    (dolist (default-command unity-lower-nodes-default-command-list)
      (when (string= (upcase cmd) (upcase default-command))
        (throw 'break default-command)))
    cmd))

(defun test-unity-replace-command-defaults-returns-TRUE-for-true ()
  (interactive)
  (if (string= (unity-replace-command-defaults "true") "TRUE")
      (message "TEST formatted correctly")
    (message "ERROR! formatted incorrectly!")))

(defun test-unity-replace-command-defaults-returns-FALSE-for-false ()
  (interactive)
  (if (string= (unity-replace-command-defaults "false") "FALSE")
      (message "TEST formatted correctly")
    (message "ERROR! formatted incorrectly!")))

(defun test-unity-replace-command-defaults-returns-test-for-TEST ()
  (interactive)
  (if (string= (unity-replace-command-defaults "false") "FALSE")
      (message "TEST formatted correctly")
    (message "ERROR! formatted incorrectly!")))

(defun test-unity-replace-command-defaults-returns-non-default-as-is ()
  (interactive)
  (if (string= (unity-replace-command-defaults "non-default") "non-default")
      (message "TEST formatted correctly")
    (message "ERROR! formatted incorrectly!")))

(defun unity-strip-whitespace (str)
  (unity-replace-regex-in-string "^[ \t]*" "" (unity-replace-regex-in-string "[ \t]*$" "" str)))

(defun test-unity-strip-whitespace ()
  (interactive)
  (if (string= (unity-strip-whitespace "  123  ") "123")
      (message "TEST stripped space correctly")
    (message "ERROR! stripped space incorrectly!")))

(defun unity-replace-space-underscore (str)
  (unity-replace-regex-in-string "[ ]+" "_" str))

(defun test-unity-replace-space-underscore ()
  (interactive)
  (if (string= (unity-replace-space-underscore "pretty tests  report") "pretty_tests_report")
      (message "TEST stripped space correctly")
    (message "ERROR! stripped space incorrectly and returned %s !" (unity-replace-space-underscore "pretty tests  report"))))

(defun unity-heading (name depth)
  "Generate heading with name and outline depth."
  (interactive)
  (format "%s:%s:" (unity-generate-depth depth) (unity-process-lower-node name)))

(defun unity-parameter (name depth)
  "Generate heading with name and outline depth."
  (interactive)
  (format "%s- %s" (unity-generate-depth depth) name))
  
(defun unity-generate-depth(depth)
  (if (< depth 1)
      (error "unity-generate-depth() requires depth >= 1"))
  (if (> depth 20)
      (error "unity-generate-depth() requires depth <= 20"))
  (if (> depth 2)
      (concat (unity-generate-depth(- depth 1)) "  ") "" ))
      
;; Tests...

(defun test-unity-heading-with-depth-of-1 ()
  (interactive)
  (let ((temp-heading (unity-heading "project" 1)))
  (if (string= "  :project:" temp-heading)
               (message "%S generated as expected!" temp-heading)
    (message "ERROR! generated %S" temp-heading))))

(defun test-unity-heading-with-depth-of-5 ()
  (interactive)
  (let ((temp-heading (unity-heading "setting" 5)))
  (if (string= "          :setting:" temp-heading)
               (message "%S generated as expected!" temp-heading)
               (message "ERROR! generated %S" temp-heading))))
      
  (defun test-unity-generate-depth-of-1 ()
  (interactive)
  (if (string= (unity-generate-depth 4) "")
      (message "TEST: Generated expected header")
    (message "ERROR: Generated incorrect header ")))
  (defun test-unity-generate-depth-of-4 ()

    (interactive)
  (if (string= (unity-generate-depth 4) "      ")
      (message "TEST: Generated expected header")
    (message "ERROR: Generated incorrect header ")))

(defun test-unity-remove-stars ()
  (interactive)
  (if (string= " this_is_a_test" (unity-remove-stars  "***** this_is_a_test"))
      (message "TEST: Returned correct string")
      (message "ERROR! returned incorrect string!")))
 
  
