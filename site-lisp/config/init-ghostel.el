;;; -*- lexical-binding: t; coding: utf-8; -*-
;;; init-ghostel.el --- Configuration for ghostel

;;; Require
(require 'ghostel)
(require 'lazy-load)

;;; Code:
(when sys/windows-p 
  (setq ghostel-shell "pwsh.exe")) 
;; 终端类型
(setq ghostel-term "xterm-ghostty")
;; url检测，可点击
(setq ghostel-enable-url-detection t)
;; 文件引用检测
(setq ghostel-enable-file-detection t)
;; 滚动缓冲区大小（默认 5MB）
(setq ghostel-max-scrollback (* 10 1024 1024))  ; 10MB
;; 退出时是否关闭 buffer
(setq ghostel-kill-buffer-on-exit t)
;; 允许终端程序写入剪贴板（SSH 远程复制很有用）
(setq ghostel-enable-osc52 t)
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

;;; --- 颜色/主题 ---

;; 终端使用独立背景（比 Emacs 背景稍深一点，视觉区分）
(set-face-attribute 'ghostel-default nil
                    :background "#242525"
                    :foreground "#00CE00"
                    :family "JetBrains Mono")

;; 粗体使用 bright 色
(setq ghostel-bold-color 'bright)

;; ANSI 16 色 — 匹配 lazycat-dark 色系
(custom-set-faces
 '(ghostel-color-black   ((t (:foreground "#3f444a" :background "#3f444a"))))
 '(ghostel-color-red     ((t (:foreground "#ff6c6b" :background "#ff6c6b"))))
 '(ghostel-color-green   ((t (:foreground "#98be65" :background "#98be65"))))
 '(ghostel-color-yellow  ((t (:foreground "#ECBE7B" :background "#ECBE7B"))))
 '(ghostel-color-blue    ((t (:foreground "#51afef" :background "#51afef"))))
 '(ghostel-color-magenta ((t (:foreground "#c678dd" :background "#c678dd"))))
 '(ghostel-color-cyan    ((t (:foreground "#46D9FF" :background "#46D9FF"))))
 '(ghostel-color-white   ((t (:foreground "#dfdfdf" :background "#dfdfdf"))))
 '(ghostel-color-bright-black   ((t (:foreground "#5B6268" :background "#5B6268"))))
 '(ghostel-color-bright-red     ((t (:foreground "#da8548" :background "#da8548"))))
 '(ghostel-color-bright-green   ((t (:foreground "#4db5bd" :background "#4db5bd"))))
 '(ghostel-color-bright-yellow  ((t (:foreground "#ECBE7B" :background "#ECBE7B"))))
 '(ghostel-color-bright-blue    ((t (:foreground "#2257A0" :background "#2257A0"))))
 '(ghostel-color-bright-magenta ((t (:foreground "#a9a1e1" :background "#a9a1e1"))))
 '(ghostel-color-bright-cyan    ((t (:foreground "#5699AF" :background "#5699AF"))))
 '(ghostel-color-bright-white   ((t (:foreground "#DFDFDF" :background "#DFDFDF")))))

;; 切换主题后自动同步终端颜色
(advice-add #'lazycat-theme-toggle :after #'ghostel-sync-theme)

(provide 'init-ghostel)

;;; init-ghostel.el ends here
