;;; -*- lexical-binding: t; coding: utf-8; -*-
;;; init-ghostel.el --- Configuration for ghostel

;;; Require
(require 'ghostel)
(require 'lazy-load)

;;; Code:
;; url检测，可点击
(setq ghostel-enable-url-detection t)
;; 文件引用检测
(setq ghostel-enable-file-detection t)
(when sys/windows-p 
  (setq ghostel-shell "pwsh.exe")) 
(setq ghostel-term "xterm-ghostty")

;; 滚动缓冲区大小（默认 5MB）
(setq ghostel-max-scrollback (* 10 1024 1024))  ; 10MB
;; 退出时是否关闭 buffer
(setq ghostel-kill-buffer-on-exit t)
;; 输入时自动滚动到底部
(setq ghostel-scroll-on-input t)

(lazy-load-unset-keys
 '("C-j")
 ghostel-mode-map)

(add-to-list 'ghostel-keymap-exceptions "C-j")

;; buffer 名称使用当前目录名而非终端标题
(defun ghostel--set-title-directory (_title)
  "Use current directory as ghostel buffer name."
  (when (or (null ghostel--managed-buffer-name)
            (equal (buffer-name) ghostel--managed-buffer-name))
    (let ((new-name (format "*ghostel: %s*"
                            (file-name-nondirectory
                             (directory-file-name default-directory)))))
      (rename-buffer new-name t)
      (setq ghostel--managed-buffer-name (buffer-name)))))

(setq ghostel-set-title-function #'ghostel--set-title-directory)

(defun ghostel@always-fresh (orig-fn &optional arg)
  "Always create a new ghostel buffer when no prefix arg given."
  (funcall orig-fn (or arg '(4))))

(advice-add #'ghostel :around #'ghostel@always-fresh)

(provide 'init-ghostel)

;;; init-ghostel.el ends here
