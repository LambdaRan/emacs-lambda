;;; init-fingertip.el --- Fingertip configuration

;; Filename: init-fingertip.el
;; Description: Fingertip configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:41:55
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:41:55
;;           By: Andy Stewart
;; URL:
;; Keywords: fingertip
;; Compatibility: GNU Emacs 23.0.60.1
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
;; Fingertip configuration
;;

;;; Installation:
;;
;; Put init-fingertip.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-fingertip)
;;
;; No need more.

;;; Change log:
;;
;; 2008/10/20
;;      First released.
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
(require 'lazy-load)

;;; Code:
(add-hook 'prog-mode-hook
          #'(lambda ()
              (require 'fingertip)
              (fingertip-mode 1)))

(with-eval-after-load 'fingertip 
;;; ### Fingertip ###
;;; --- 结构化编程  
(lazy-load-unset-keys
 '("M-J" "M-r" "M-s" "M-;" "C-M-f" "C-M-b" "M-)")
 fingertip-mode-map)                    ;卸载按键
(defvar fingertip-key-alist nil)
(setq fingertip-key-alist
      '(
        ;; 移动
        ("M-n" . fingertip-jump-left)
        ("M-p" . fingertip-jump-right)
        ;; 符号插入
        ("%" . fingertip-match-paren)       ;括号跳转
        ("(" . fingertip-open-round)        ;智能 (
        ("[" . fingertip-open-bracket)      ;智能 [
        ("{" . fingertip-open-curly)        ;智能 {
        (")" . fingertip-close-round)       ;智能 )
        ("]" . fingertip-close-bracket)     ;智能 ]
        ("}" . fingertip-close-curly)       ;智能 }
        ("\"" . fingertip-double-quote)     ;智能 "
        ("'" . fingertip-single-quote)      ;智能 '
        ("=" . fingertip-equal)             ;智能 =
        ("SPC" . fingertip-space)           ;智能 space
        ("RET" . fingertip-newline)         ;智能 newline
        ;; 删除
        ("M-o" . fingertip-backward-delete) ;向后删除
        ("C-d" . fingertip-forward-delete)  ;向前删除
        ("C-k" . fingertip-kill)            ;向前kill
        ;; 包围
        ("M-\"" . fingertip-wrap-double-quote) ;用 " " 包围对象, 或跳出字符串
        ("M-'" . fingertip-wrap-single-quote) ;用 ' ' 包围对象, 或跳出字符串
        ("M-[" . fingertip-wrap-bracket)      ;用 [ ] 包围对象
        ("M-{" . fingertip-wrap-curly)        ;用 { } 包围对象
        ("M-(" . fingertip-wrap-round)        ;用 ( ) 包围对象
        ("M-)" . fingertip-unwrap)            ;去掉包围对象
        ;; 跳出并换行缩进
        ("M-:" . fingertip-jump-out-pair-and-newline) ;跳出括号并换行
        ;; 向父节点跳动
        ;; ("C-j" . fingertip-jump-up)
        ))
(lazy-load-set-keys fingertip-key-alist fingertip-mode-map)  
)

(provide 'init-fingertip)

;;; init-fingertip.el ends here
