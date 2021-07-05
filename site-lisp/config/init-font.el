;;; init-font.el --- Font configuration

;; Filename: init-font.el
;; Description: Font configuration
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2020, Andy Stewart, all rights reserved.
;; Created: 2020-03-22 11:16:26
;; Version: 0.1
;; Last-Updated: 2020-03-22 11:16:26
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-font.el
;; Keywords:
;; Compatibility: GNU Emacs 26.3
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
;; Font configuration
;;

;;; Installation:
;;
;; Put init-font.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-font)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-font RET
;;

;;; Change log:
;;
;; 2020/03/22
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
(require 'lazy-load)

;;; Code:
(let ((emacs-font-size 14)
      emacs-font-name)
  (cond
    (sys/windows-p
     (setq emacs-font-size 10)
     (setq emacs-font-name "JetBrains Mono"))
    (sys/mac-cocoa-p
     (setq emacs-font-name "JetBrains Mono"))
    (sys/linux-p
     ;; (setq emacs-font-name "WenQuanYi Micro Hei Mono")
     (setq emacs-font-name "Source Code Pro"))
    (t (message "Other System OS, Please configure this case.")))
  (when (display-grayscale-p)
    (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size)))
    ;; (set-fontset-font (frame-parameter nil 'font) 'unicode (eval emacs-font-name))
    ))

;; 给相应的字符集设置中文字体，这里的字体是。
(when (display-graphic-p)
  (dolist (charset '(kana han cjk-misc bopomofo chinese-gbk))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "微软雅黑" :size 20))))

;; (setq-default line-spacing 0.2)

;;; ### Font ###
;;; --- 字体命令
(lazy-load-set-keys
 '(
   ("s--" . text-scale-decrease)        ;减小字体大小
   ("s-=" . text-scale-increase)        ;增加字体大小
   ))

(provide 'init-font)

;;; init-font.el ends here