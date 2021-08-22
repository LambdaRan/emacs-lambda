;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-generic.el --- Generic config

;; Filename: init-generic.el
;; Description: Generic config
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-20 23:57:56
;; Version: 0.1
;; Last-Updated: 2014-01-20 23:57:56
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-generic.el
;; Keywords:
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Generic config
;;

;;; Installation:
;;
;; Put init-generic.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-generic)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-generic RET
;;

;;; Change log:
;;
;; 2014/01/20
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require

(require 'init-const)
;;; Code:

;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)

(setq jit-lock-defer-time 0)

;; Restore emacs session.
(setq initial-buffer-choice t)
(run-with-timer 1 nil #'(lambda () (bury-buffer)))

;; 增加长行处理性能
(setq bidi-inhibit-bpa t)
(setq-default bidi-paragraph-direction 'left-to-right)

;; 增加IO性能
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 1024 1024))

(fset 'yes-or-no-p 'y-or-n-p)           ;以 y/n代表 yes/no
(setq use-dialog-box nil)               ;never pop dialog
(setq inhibit-startup-screen t)         ;inhibit start screen
(setq initial-scratch-message "")       ;关闭启动空白buffer, 这个buffer会干扰session恢复
(setq-default comment-style 'indent)    ;设定自动缩进的注释风格
(setq ring-bell-function 'ignore)       ;关闭烦人的出错时的提示声
(setq default-major-mode 'text-mode)    ;设置默认地主模式为TEXT模式
(setq mouse-yank-at-point t)            ;粘贴于光标处,而不是鼠标指针处
(setq x-select-enable-clipboard t)      ;支持emacs和外部程序的粘贴
(setq split-width-threshold nil)        ;分屏的时候使用上下分屏
(setq inhibit-compacting-font-caches t) ;使用字体缓存，避免卡顿
(setq confirm-kill-processes nil)       ;退出自动杀掉进程
(setq async-bytecomp-allowed-packages nil) ;避免magit报错
(setq word-wrap-by-category t)             ;按照中文折行
(setq profiler-report-cpu-line-format ;让 profiler-report 第一列宽一点
      '((100 left)
        (24 right ((19 right)
                   (5 right)))))
(setq profiler-report-memory-line-format
      '((100 left)
        (19 right ((14 right profiler-format-number)
                   (5 right)))))
(add-hook 'find-file-hook 'highlight-parentheses-mode t) ;增强的括号高亮

(setq ad-redefinition-action 'accept)   ;不要烦人的 redefine warning
(setq frame-resize-pixelwise t) ;设置缩放的模式,避免Mac平台最大化窗口以后右边和下边有空隙
(setq delete-by-moving-to-trash t)      ; 删除的文件移动到垃圾篓
(setq switch-to-buffer-preserve-window-point t)
;; (set-cursor-color "Red")           ;更改光标颜色
(add-to-list 'default-frame-alist '(cursor-color . "Red"))
(setq-default cursor-type 'bar)    ;更改光标类型

;; 平滑地进行半屏滚动，避免滚动后recenter操作
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; ;; 不显示 *scratch*
;; (defun remove-scratch-buffer ()
;;  (if (get-buffer "*scratch*")
;;      (kill-buffer "*scratch*")))
;; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(when sys/windows-p
  ;; 使用英文day-time
  (setq system-time-locale "C")
  (set-default 'process-coding-system-alist
               '(("[cC][mM][dD][pP][rR][oO][xX][yY]" gbk-dos . gbk-dos))))

;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)

(unless sys/windows-p
  (setq locale-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (set-clipboard-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (modify-coding-system-alist 'process "*" 'utf-8)

  ;; Set LANG and LC_* variables
  (setenv "LC_ALL" "en_US.UTF-8")
  (setenv "LANG" "en_US.UTF-8")
  (setenv "LC_CTYPE" "en_US.UTF-8"))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html#Recognize-Coding
(prefer-coding-system 'gb18030)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8)


(provide 'init-generic)

;;; init-generic.el ends here
