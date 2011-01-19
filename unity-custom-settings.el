
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

(provide 'unity-custom-settings)
 
