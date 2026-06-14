;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'init-const)
(require 'init-accelerate)

;;; 增量包加载器
;; Ref: Doom Emacs modules/doom/init.el:652-695
;; 在空闲间隙逐包加载，通过 `current-idle-time' 和 `while-no-input'
;; 检测用户操作，避免输入时卡顿。
(defvar my-incremental-packages nil
  "待增量加载的 feature 列表。")

(defvar my-incremental-first-idle-timer 0
  "开始增量加载前的空闲秒数。用户空闲多久后开始加载第一个包。")

(defvar my-incremental-idle-timer 0.2
  "每两个包之间的空闲秒数。加载完一个包后，等多久加载下一个。")

(defun my-load-packages-incrementally (packages &optional now)
  "在空闲间隙逐个加载 PACKAGES。
NOW 非 nil 时立即开始加载，否则注册待后续加载。"
  (let ((gc-cons-threshold most-positive-fixnum))
    (if (not now)
        (setq my-incremental-packages
              (append my-incremental-packages packages))
      (while packages
        (let ((req (pop packages))
              idle-time)
          (if (featurep req)
              nil  ; 已加载，跳过
            (condition-case-unless-debug e
                (and (or (null (setq idle-time (current-idle-time)))
                         (< (float-time idle-time)
                            my-incremental-first-idle-timer)
                         (not (while-no-input
                                (require req nil t)
                                t)))
                     (push req packages))  ; 用户正在操作，重新入队
              (error (message "增量加载 %S 失败: %s" req e)))
            (if (null packages)
                nil  ; 全部加载完成
              (run-at-time (if idle-time
                               my-incremental-idle-timer
                             my-incremental-first-idle-timer)
                           nil #'my-load-packages-incrementally
                           packages t)
              (setq packages nil))))))))


;;; 启动优化
;; Ref: Doom Emacs early-init.el:131-171, 222-237, 347

;;; 命名函数：替代 emacs-startup-hook 中逻辑较复杂的匿名 lambda

(defvar my--saved-fnha nil "启动前保存的 file-name-handler-alist。")

(defun my-restore-fnha ()
  "启动后恢复 file-name-handler-alist（合并而非覆盖）。"
  (set-default-toplevel-value
   'file-name-handler-alist
   (delete-dups
    (append (default-toplevel-value 'file-name-handler-alist)
            my--saved-fnha))))

;; PERF: 启动期间保存并简化 `file-name-handler-alist'。
;; 每次 `require'、`load'、`expand-file-name' 等都会查询它。
;; Ref: Doom Emacs early-init.el:131-148
;; 使用 setq 而非 let，避免 lexical-binding 下词法作用域遮蔽 defvar。
(setq my--saved-fnha (default-toplevel-value 'file-name-handler-alist))
(set-default-toplevel-value
 'file-name-handler-alist
 ;; 如果内置 .el 文件未压缩，可移除所有 handler 以获得最大启动速度；
 ;; 如果压缩了，保留 gzip handler。
 (if (locate-file-internal "calc-loaddefs.el" load-path)
     nil
   (list (rassq 'jka-compr-handler my--saved-fnha))))
(add-hook 'emacs-startup-hook #'my-restore-fnha 101)

;; PERF: 启动期间精简 `load-suffixes'，跳过 .so/.dll 检查。
;; DLL 包（vterm、ghostel）在启动后的空闲时加载，不受影响。
;; Ref: Doom Emacs early-init.el:337-355
(let ((load-suffixes '(".elc" ".el")))

  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            #'(lambda ()
                (setq-default inhibit-redisplay nil
                              inhibit-message nil)
                (redisplay)))

  (add-hook 'emacs-startup-hook
            #'(lambda ()
                (advice-remove #'tool-bar-setup #'ignore)))

  (with-temp-message ""
    ;; === 同步加载：核心基础（必须立即就绪） ===
    (require 'lazy-load)           ; lazy-load 宏，后续 require 依赖
    (require 'one-key)             ; one-key 库，多个模块依赖
    (require 'init-font)           ; 字体（用户立即看到）
    (require 'init-generic)        ; 基础设置（编码、滚动、光标等）
    (require 'init-startup)        ; 禁用 toolbar/menu/scrollbar
    (require 'init-theme)          ; 主题（用户立即看到）
    (when (featurep 'cocoa)
      (require 'cache-path-from-shell)
      (require 'exec-path-from-shell)
      (exec-path-from-shell-initialize))
    (require 'init-mode)           ; auto-mode-alist（文件模式映射）
    (require 'init-treesit)        ; tree-sitter（major-mode-remap-alist）
    (require 'init-auto-save)      ; 自动保存
    ;; === 同步加载：UI 组件 ===
    (require 'init-awesome-tab)    ; 标签栏
    (require 'init-awesome-tray)   ; 状态栏
    (require 'init-key)            ; 按键绑定
    (require 'init-ivy)            ; 补全框架

    ;; === 增量加载：用户空闲时逐包加载 ===
    ;; Ref: Doom Emacs modules/doom/init.el:652-695
    (add-hook 'emacs-startup-hook
              #'(lambda ()
                  (my-load-packages-incrementally
                   '(;; 第一批：视觉反馈（轻量，用户立即感知）
                     init-highlight-parentheses
                     init-line-number
                     init-company-mode
                     init-fingertip
                     ;; 第二批：导航与编辑（用户高频操作）
                     init-dired
                     init-one-key
                     init-vi-navigate
                     init-treesit-fold
                     init-indent
                     ;; 第三批：工具（按需使用）
                     init-ffip
                     init-color-rg
                     init-fastctags
                     init-tempbuf
                     init-info
                     init-c
                     ran-toolkit
                     ;; 第四批：重型模块（终端、AI 配置）
                     init-agent-shell
                     init-ghostel
                     ;; 最后恢复会话
                     init-session)
                   t)))

    ;; 会话恢复：在 init-session 加载后触发
    ;;（由增量加载器或手动加载）。
    (with-eval-after-load 'init-session
      (emacs-session-restore))

    ) ;; end with-temp-message
  ) ;; end let

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Emacs ready in %.2f seconds with %d garbage collections."
                       (float-time (time-subtract after-init-time before-init-time))
                       gcs-done)))

(provide 'init)
