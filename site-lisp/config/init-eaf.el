;;; init-eaf.el --- Configuration for eaf

;;; Require
(require 'eaf)
(require 'eaf-pyqterminal)

;;; Code:
(when sys/windows-p
  (setq eaf-python-command "C:\\EafPythonVirtualEnvironment\\.venv\\Scripts\\python.exe"))

(setq eaf-rebuild-buffer-after-crash nil)
(setq eaf-goto-right-after-close-buffer t)

(setq eaf-pyqterminal-font-family "FiraCode Nerd Font Mono")
(setq eaf-pyqterminal-font-size 16)
(setq eaf-pyqterminal-cursor-type "hbar")
(setq eaf-pyqterminal-cursor-size 2)
(setq eaf-pyqterminal-refresh-ms 16)

(eaf-bind-key eaf-send-backspace-key "M-o" eaf-pyqterminal-keybinding)
(eaf-bind-key ace-window "C-j" eaf-pyqterminal-keybinding)

(defun eaf-open-terminal ()
  "Try to open fish if fish exist, otherwise use default shell."
  (interactive)
  (eaf-pyqterminal-run-command-in-dir
   (if (executable-find "fish")
       "fish"
     (eaf--generate-terminal-command))
   (eaf--non-remote-default-directory)
   t))

(provide 'init-eaf)

;;; init-eaf.el ends here
