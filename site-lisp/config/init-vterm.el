;;; init-vterm.el --- Configuration for vterm

;;; Require
(require 'vterm)
(require 'lazy-load)

;;; Code:

;; (when (eq system-type 'windows-nt)
;;   (setq vterm-shell "powershell"))

(lazy-load-unset-keys
 '("C-j")
 vterm-mode-map)

(defun vterm-new ()
  "创建一个新的 vterm buffer，每次调用都会创建新的实例。"
  (interactive)
  (let ((buf (generate-new-buffer vterm-buffer-name)))
    (pop-to-buffer-same-window buf)
    (with-current-buffer buf
      (unless (derived-mode-p 'vterm-mode)
        (vterm-mode)))
    buf))

(provide 'init-vterm)

;;; init-vterm.el ends here
