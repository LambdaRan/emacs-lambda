;;; init-ghostel.el --- Configuration for ghostel

;;; Require
(require 'ghostel)
(require 'lazy-load)

;;; Code:
;; url检测，可点击
(setq ghostel-enable-url-detection t)
;; 文件引用检测
(setq ghostel-enable-file-detection t)

(setq ghostel-shell "pwsh.exe")

(lazy-load-unset-keys
 '("C-j")
 ghostel-mode-map)

(add-to-list 'ghostel-keymap-exceptions "C-j")

;; 缩短终端 buffer 名称，只保留最后一级目录名
(defun ghostel--set-title (title)
  "Update the buffer name with TITLE from the terminal."
  (rename-buffer (format "*ghostel: %s*"
                         (file-name-nondirectory
                          (directory-file-name title)))
                 t))

(provide 'init-ghostel)

;;; init-ghostel.el ends here
