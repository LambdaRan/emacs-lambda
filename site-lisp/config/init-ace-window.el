;; URL: https://github.com/abo-abo/ace-window

;;; Require
(require 'ace-window)

;;; Code:

;; 关闭灰色背景
(setq aw-background nil)

;; (defvar one-key-menu-ace-windows-alist nil
;;   "The `one-key' menu alist for ace-window.")

;; (setq one-key-menu-ace-windows-alist
;;     '(
;;         (("t" . "Swap-Windows") . aw-swap-window)
;;         (("m" . "Move-Window") . aw-move-window)
;;         (("c" . "Copy-Window") . aw-copy-window)
;;         (("s" . "Select-Buffer") . menu-bar-mode)

;;         (("u" . "Switch-Buffer-Other-Window") . aw-switch-buffer-other-window)

;;         (("f" . "Split-Fair-Window") . aw-split-window-fair)
;;         (("v" . "Split-Vert-Window") . aw-split-window-vert)
;;         (("h" . "Split-Horz-Window") . aw-split-window-horz)
;;         (("d" . "Delete-Window") . aw-delete-window)
;;         (("o" . "Delete-Other-Windows") . delete-other-windows)

;;         (("n" . "Flip-Buffer") . aw-flip-window)
;;         (("H" . "Show-Dispatch-Help") . aw-show-dispatch-help)      
;;         ))
;; (defun one-key-menu-ace-windows ()
;;   "The `one-key' menu for ace-windows."
;;   (interactive)
;;   (one-key-menu "ace-window" one-key-menu-ace-windows-alist t))

(provide 'init-ace-window)