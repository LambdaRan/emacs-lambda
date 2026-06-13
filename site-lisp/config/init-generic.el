;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-generic.el --- Generic config

(require 'init-const)

;;; Code:
(setq frame-title-format "Emacs")
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)
(setq jit-lock-defer-time 0)
;; Restore emacs session.
(setq initial-buffer-choice t)
(run-with-timer 1 nil #'(lambda () (bury-buffer)))

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

(provide 'init-generic)

;;; init-generic.el ends here
