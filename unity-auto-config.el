(provide 'unity-auto-config)

(defun unity-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun unity-insert-heading(text)
  (insert "\n" (unity-colour text  "light blue") "\n"))

(defun unity-display-is-this-correct? ()
  (insert
   (unity-colour   "  Is this correct? (" "yellow")
   (unity-colour "ENTER " "green")
   (unity-colour "confirms , " "yellow")
   (unity-colour "else correct in mini-buffer)" "yellow")))

(defun unity-display-config-setting (buffer parameter param-name text)
  (let ((start (point)) (temp nil))
    (let ((inhibit-read-only t))
      (buffer-disable-undo)
      (insert
       (unity-colour (concat "\n" text "\n\n") "yellow")
       "  " parameter "\n\n")
      (unity-display-is-this-correct?))
    (let ((inhibit-read-only nil))
      (setq temp
            (completing-read (concat
                              (unity-chomp param-name) " ")
                             parameter nil nil parameter )))
    (let ((inhibit-read-only t))
      (backward-delete-char (- (point-at-eol) start))
      temp)))

(defun unity-display-updated-config-setting (buffer parameter param-name)
  (let ((inhibit-read-only t))
    (insert
     (unity-colour param-name "green")
     "  " parameter "\n")))

(defun unity-config-setting (buffer parameter param-name text)
  (if (not parameter) (setq parameter ""))
  (setq parameter
        (unity-display-config-setting
         buffer parameter param-name text))
  (unity-display-updated-config-setting
   buffer parameter param-name)
  parameter)

(defun unity-setup ()
  (interactive)
  (save-excursion
    (setq buffer-name unity-temp-buffer)
    (let ((buffer (get-buffer-create buffer-name)))
      (switch-to-buffer buffer)
      (with-current-buffer buffer
        (setq buffer-read-only t)
        (let ((inhibit-read-only t))
          (buffer-disable-undo)
          (erase-buffer)
          (unity-insert-heading
           "           Unity Mode Project Setup...")
          (unity-insert-heading "Directories...")

          (setq
           unity-project-root-dir
           (unity-config-setting
            buffer 
            unity-project-root-dir
            "project-root:   "
            "The Project Root Directory appears to be..."))

          (setq
           unity-ceedling-root-dir
           (unity-config-setting
            buffer
            unity-ceedling-root-dir
            "ceedling-root:  "
            "The Ceedling Root Directory appears to be..."))

          (setq
           unity-unity-root-dir
           (unity-config-setting
            buffer
            unity-unity-root-dir
            "unity-root:     "
            "The Unity Root Directory appears to be..."))

          (setq
           unity-cmock-root-dir
           (unity-config-setting
            buffer
            unity-cmock-root-dir
            "cmock-root:     "
            "The CMock Root Directory appears to be..."))

          (setq
           unity-plugins-dir
           (unity-config-setting
            buffer
            unity-plugins-dir
            "plugins-dir:    "
            "The Plugins Directory appears to be..."))

          (setq
           unity-custom-plugins-dir
           (unity-config-setting
            buffer
            unity-custom-plugins-dir
            "custom-plugins: "
            "The Custom Plugins Directory appears to be..."))

          (setq
           unity-src-dir
           (unity-config-setting
            buffer
            unity-src-dir
            "C-source-dir:   "
            "The Source Directory appears to be..."))
          
          (setq
           unity-header-dir
           (unity-config-setting
            buffer 
            unity-header-dir
            "header-dir:     "
            "The Header Directory appears to be..."))
          
          (setq
           unity-test-dir
           (unity-config-setting
            buffer
            unity-test-dir
            "test-dir:       "
            "The Test Directory appears to be..."))

          (setq
           unity-mocks-dir
           (unity-config-setting
            buffer
            unity-mocks-dir
            "mocks-dir:      "
            "The Mocks Directory appears to be..."))

          (setq
           unity-build-dir
           (unity-config-setting
            buffer
            unity-build-dir
            "build-dir:      "
            "The Build Directory appears to be..."))

          (unity-insert-heading "Extensions...")

          (setq
           unity-src-file-extension
           (unity-config-setting
            buffer
            unity-src-file-extension
            "source file extension:"
            "The source file extension appears to be..."))

          (setq
           unity-header-file-extension
           (unity-config-setting
            buffer
            unity-header-file-extension
            "header file extension:"
            "The header file extension appears to be..."))

          (unity-insert-heading "File prefixes...")

          (setq
           unity-test-file-prefix
           (unity-config-setting
            buffer
            unity-test-file-prefix
            "test file prefix:     "
            "The test file prefix appears to be..."))

          (setq
           unity-mock-file-prefix
           (unity-config-setting
            buffer
            unity-mock-file-prefix
            "mock file prefix:     "
            "The mock file prefix appears to be..."))

          (unity-insert-heading "MCH Filename suffixes...")

          (setq
           unity-model-file-suffix
           (unity-config-setting
            buffer
            unity-model-file-suffix
            "model file suffix:    "
            "The Model file suffix appears to be..."))

          (setq
           unity-conductor-file-suffix
           (unity-config-setting
            buffer
            unity-conductor-file-suffix
            "conductor file suffix:"
            "The Conductor file suffix appears to be..."))

          (setq
           unity-hardware-file-suffix
           (unity-config-setting
            buffer
            unity-hardware-file-suffix
            "hardware file suffix: "
            "The Hardware file suffix appears to be..."))

          (setq
           unity-configurator-file-suffix
           (unity-config-setting
            buffer
            unity-configurator-file-suffix
            "configurator suffix:  "
            "The Configurator file suffix appears to be..."))

          (unity-insert-heading "Directory/File Generation...")

          (let ((reply "yes"))
            (setq reply
                  (if (unity-config-setting
                       buffer
                       reply
                       "generate missing directories:"
                       "Auto-Generate missing directories?")
                      ())))

          (let ((reply "yes"))
            (setq reply
                  (if (unity-config-setting
                       buffer
                       reply
                       "generate rakefile:           "
                       "Auto-Generate rakefile?")
                      ())))

          (let ((reply "yes"))
            (setq reply
                  (if (unity-config-setting
                       buffer
                       reply
                       "generate project.org:        "
                       "Auto-Generate project.org?")
                      ())))
          
          (let ((reply "yes"))
            (setq reply
                  (if (unity-config-setting
                       buffer
                       reply
                       "generate project.yml:        "
                       "Auto-Generate project.yml?")
                      ())))
          
        (unity-insert-heading "           Project Setup Complete - Hit ENTER..."))
        (read-from-minibuffer "Finished! Hit ENTER..."))
      (kill-buffer unity-temp-buffer))))


