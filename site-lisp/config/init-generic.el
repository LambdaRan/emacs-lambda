;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-generic.el --- Generic config

(require 'init-const)

;;; Code:
(setq frame-title-format "Emacs")
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)
(setq jit-lock-defer-time 0.05)
;; Restore emacs session.
(setq initial-buffer-choice t)
(run-with-timer 1 nil (lambda ()
  (let ((buf (get-buffer "*scratch*")))
    (when (eq (current-buffer) buf)
      (bury-buffer)))))

(fset 'yes-or-no-p #'y-or-n-p)           ;以 y/n代表 yes/no
(setq use-dialog-box nil)               ;never pop dialog
(setq inhibit-startup-screen t)         ;inhibit start screen
(setq initial-scratch-message "")       ;关闭启动空白buffer, 这个buffer会干扰session恢复
(setq-default comment-style 'indent)    ;设定自动缩进的注释风格
(setq ring-bell-function #'ignore)       ;关闭烦人的出错时的提示声
(setq mouse-yank-at-point t)            ;粘贴于光标处,而不是鼠标指针处
(setq select-enable-clipboard t)        ;支持emacs和外部程序的粘贴
(setq split-width-threshold nil)        ;分屏的时候使用上下分屏
(setq confirm-kill-processes nil)       ;退出自动杀掉进程
(setq async-bytecomp-allowed-packages nil) ;避免magit报错
(setq word-wrap-by-category t)             ;按照中文折行
(setq profiler-report-cpu-line-format ;让 profiler-report 第一列宽一点
      '((24 right ((19 right)
                   (5 right)))
        (1 left "%s")
        (0 left)))
(setq profiler-report-memory-line-format
      '((19 right ((14 right profiler-format-number)
                   (5 right)))
        (1 left "%s")
        (0 left)))
(setq completion-auto-select nil)       ;避免默认自动选择
(setq ad-redefinition-action 'accept)   ; 不要烦人的 redefine warning
(setq frame-resize-pixelwise t) ; 设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
(setq delete-by-moving-to-trash t)      ; 删除的文件移动到垃圾篓
(setq switch-to-buffer-preserve-window-point t)
(setq-default cursor-type 'bar)         ;更改光标类型
(add-to-list 'default-frame-alist '(cursor-color . "Red"))

;; 平滑地进行半屏滚动，避免滚动后recenter操作
(setq scroll-step 1)
(setq scroll-conservatively 10000)
;; 像素滚动
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-large-scroll-height 60)
(setq pixel-scroll-precision-interpolation-factor 8.0)
;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(when sys/windows-p
  ;; 禁用双buff
  (setq w32-disable-double-buffering t)
  ;; 使用英文day-time
  (setq system-time-locale "C"))

;; prefer-coding-system 后调用的优先级更高；不要用 set-language-environment，会清除 GBK 检测
;; 项目级编码优先级通过 .dir-locals.el 中 (eval . (prefer-coding-system 'xxx)) 覆盖
(if sys/windows-p
    (progn
      (prefer-coding-system 'utf-8)
      (prefer-coding-system 'gb18030)
      (set-default-coding-systems 'gb18030))
  (prefer-coding-system 'gb18030)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8))

;; 基础编辑模式（从 init-idle.el 移入，确保启动即可用）
(tooltip-mode -1)                   ; 不要显示任何 tooltips
(show-paren-mode t)                 ; 显示括号匹配
(global-hl-line-mode 1)             ; 高亮当前行
(delete-selection-mode t)           ; 选中文本可以编辑
(blink-cursor-mode -1)              ; 指针不闪动
(transient-mark-mode 1)             ; 标记高亮
(electric-pair-mode t)              ; 括号自动匹配插入
(global-auto-revert-mode t)         ; 文件被外部程序修改后自动重新加载
(global-subword-mode t)             ; Word移动支持 FooBar 的格式

;;; --- 基础行为设置（原 init-idle.el，均为内建变量，无需延迟加载） ---

;; Ring 与历史记录
(setq kill-ring-max 1024)               ;用一个很大的 kill ring，防止误删重要内容
(setq mark-ring-max 1024)               ;mark ring 容量
(setq global-mark-ring-max 1024)        ;全局 mark ring 容量
(setq history-delete-duplicates t)      ;删除 minibuffer 的重复历史

;; Lisp 求值与编译
(setq max-lisp-eval-depth 4000)         ;lisp 最大执行深度
(setq max-specpdl-size 4000)            ;最大容量
(setq eval-expression-print-length nil) ;执行表达式的长度没有限制
(setq eval-expression-print-level nil)  ;执行表达式的深度没有限制
(setq read-quoted-char-radix 16)        ;引用字符的基数
(setq byte-compile-warnings
      '(
        free-vars                 ;不在当前范围的引用变量
        unresolved                ;不知道的函数
        callargs                  ;函数调用的参数和定义的不匹配
        obsolete                  ;荒废的变量和函数
        noruntime                 ;函数没有定义在运行时期
        interactive-only          ;正常不被调用的命令
        make-local                ;调用 `make-variable-buffer-local' 可能会不正确的
        mapcar                    ;`mapcar' 调用
        (not redefine)            ;重新定义的函数
        (not cl-functions)        ;`CL' 包中的运行时调用的函数
        ))

;; 搜索与 minibuffer
(setq isearch-allow-prefix t)           ;isearch 搜索时允许前缀操作
(setq isearch-lazy-count t
      lazy-count-prefix-format "%s/%s ") ;isearch 搜索显示匹配个数
(setq enable-recursive-minibuffers t)   ;minibuffer 递归调用命令
(setq minibuffer-message-timeout 1)     ;显示消息超时的时间

;; 括号与光标
(setq show-paren-style 'parentheses)    ;括号匹配显示但不跳到另一个括号
(setq blink-matching-paren nil)         ;插入右括号时不显示匹配的左括号
(setq x-stretch-cursor t)               ;光标在 TAB 字符上显示为大方块

;; 文件与编辑
(setq require-final-newline nil)        ;不自动添加换行符到末尾
(setq message-log-max t)                ;message 记录全部消息
(setq print-escape-newlines t)          ;显示字符窗中的换行符为 \n
(setq void-text-area-pointer nil)       ;禁止显示鼠标指针

;; 远程与传输
(setq tramp-verbose 0)                  ;关闭 tramp 消息
(setq tramp-default-method "ssh")       ;传送文件默认方法

;; UI 微调
(setq echo-keystrokes 0.1)              ;加快快捷键提示的速度
(setq one-key-popup-window nil)         ;禁止自动弹出窗口

;; 包管理镜像
(setq package-archives
      '(("gnu"   . "https://elpa.emacs-china.org/gnu/")
        ("melpa" . "https://elpa.emacs-china.org/melpa/")))

;; Ediff 窗口设置
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

(provide 'init-generic)

;;; init-generic.el ends here
