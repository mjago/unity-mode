(defcustom unity-rake-command "rake"
  "The command for rake"
  :type 'string
  :group 'unity-mode)
(defcustom unity-project-root-dir
  "~/.emacs.d/martyn/martyn/unity-mode/ceedling/trunk/examples/temp_sensor/"
  "Project Root Directory"
:type 'string
:group 'unity-mode)
(defcustom unity-ceedling-root-dir
  (concat unity-project-root-dir "../../")
  "Ceedling Root Directory"
  :type 'string
  :group 'unity-mode)
(defcustom unity-unity-root-dir
  (concat unity-ceedling-root-dir "vendor/unity/")
          "Unity Root Directory"
:type 'string
:group 'unity-mode)
(defcustom unity-cmock-root-dir
  (concat unity-ceedling-root-dir "vendor/cmock/")
  "CMock Root Directory"
:type 'string
:group 'unity-mode)
(defcustom unity-plugins-dir
  (concat unity-ceedling-root-dir "plugins/")
          "Plugins Directory"
:type 'string
:group 'unity-mode)
(defcustom unity-custom-plugins-dir
  (concat unity-ceedling-root-dir "custom-plugins/")
  "Custom Plugins Directory"
:type 'string
:group 'unity-mode)
(defcustom unity-src-dir
  (concat unity-project-root-dir "src/")
          "Source Directory"
:type 'string
:group 'unity-mode)
(defcustom unity-test-dir
  (concat unity-project-root-dir "test/")
  "Test Files Directory"
:type 'string
:group 'unity-mode)
(defcustom unity-header-dir
  (concat unity-project-root-dir "src/")
  "Header Files Directory"
:type 'string
:group 'unity-mode)
(defcustom
  unity-mocks-dir
  (concat unity-project-root-dir "mocks/")
  "Mock Files Directory"
:type 'string
:group 'unity-mode)
(defcustom
  unity-build-dir
  (concat unity-ceedling-root-dir "build/")
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

(provide 'unity-custom-settings)
 
