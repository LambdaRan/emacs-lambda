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

;;; --- 程序状态 / 提醒 ---

;; 全局 mode line 汇总各终端程序状态（OSC 7501 / ConEmu 9;4 进度），
;; 悬停显示终端名，点击跳到最近需要关注的终端；无状态时不显示。
;; 终端自身的 mode line 也会显示状态，无需额外配置。
(ghostel-status-indicator-mode 1)
;; 切到该终端即清掉 done/error（上游默认 'input：按键才清）
(setq ghostel-status-auto-acknowledge 'seen)
;; 不用 spinner.el 动画（未装 spinner 时上游本来也忽略，这里显式关闭）
(setq ghostel-spinner-type nil)
;; 状态标签默认 'emoji，帧显示不了 emoji 时 ghostel 自动退回文字标签；
;; 想强制文字标签（如 [blocked: permission x2]）就取消下面注释：
;; (setq ghostel-status-mode-line-labels 'words)

;;; --- 铃声 / 通知 ---

;; BEL 不响铃（vim/pwsh 的响铃很吵；想恢复删掉这行）
(setq ghostel-bell-functions nil)
;; OSC 9 / OSC 777 通知：tty frame 转发给外层终端，GUI frame 退回 message。
;; 装了 alert 包的话可直接用上游默认 (ghostel-default-notify)。
(setq ghostel-notification-functions '(ghostel-tty-forward-notify))

;;; --- 异步命令走真 TTY ---

;; shell-command 的异步形式（M-&、dired 的 &、project-async-shell-command、
;; 以及 Lisp 调用）统一改在 ghostel 终端里跑：有真正的 TTY，
;; 那些依赖 isatty 的程序（fzf、git 交互、彩色输出、密码提示等）才能正常工作。
(require 'ghostel-shell-command)
(ghostel-shell-command-global-mode 1)

;;; --- 中文输入 ---

;; Lisp 输入法（Quail/Chinese、Hangul）在终端 buffer 里的组合与提交
(require 'ghostel-ime)
(add-hook 'ghostel-mode-hook #'ghostel-ime-mode)

(add-to-list 'ghostel-keymap-exceptions "C-j")
;; add-to-list 不触发 defcustom 的 :set 回调，需要手动 rebuild
;; 让 semi-char keymap 重新生成时跳过 C-j
(ghostel--rebuild-semi-char-keymap)

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

(setq ghostel-buffer-name-function #'ghostel--set-title-directory)

;;; --- 快捷键 ---

;; 0.59 起内置 `ghostel-new'，取代原先用 advice 强制
;; “每次 M-x ghostel 都新建 buffer” 的做法。
;; 现在 `M-x ghostel' 恢复上游语义（有则切换、无则新建），新开用下面的键。
(keymap-global-set "C-c g n" #'ghostel-new)
;; 跳到最近需要关注的终端（配合上面的 ghostel-status-indicator-mode）
(keymap-global-set "C-c g a" #'ghostel-status-next)

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
