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

;; (setq-default line-spacing 0.2)

;; https://github.com/Eason0210/emacs.d/blob/master/lisp/init-font.el
(defvar font-list
  (cond
   (sys/mac-p
    '(("JetBrains Mono" . 150) ("SF Mono" . 150) ("Monaco" . 150)))
   (sys/windows-p
    '(("JetBrains Mono" . 100) ("Courier New" . 100) ("Consolas" . 100)))
   (t
    '(("Source Code Pro" . 100) ("Consolas" . 100) ("Cascadia Mono" . 100))))
  "List of fonts and sizes.  The first one available will be used.")

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun change-font ()
  "Set English font from the `font-list'."
  (interactive)
  (let* (available-fonts font-name font-size)
    (dolist (font font-list
             (setq available-fonts (nreverse available-fonts)))
      (when (font-installed-p (car font))
        (push font available-fonts)))
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string
                          (completing-read "What font to use? "
                                           available-fonts nil t)
                          available-fonts)))
            (setq font-name (car chosen)
                  font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts)
              font-size (cdar available-fonts)))
      (set-face-attribute 'default nil :font font-name :height font-size))))

(when (display-graphic-p)
  (change-font)
  (catch 'end
    (dolist (font '("Segoe UI Symbol" "Apple Color Emoji" "Symbola" "Symbol"))
            (when (font-installed-p font)
              (set-fontset-font t 'unicode font nil 'prepend)
              (throw 'end t))))
  (catch 'end                      ;给相应的字符集设置中文字体，这里的字体是。
    (dolist (font '("Microsoft Yahei"))
            (when (font-installed-p font))
            (dolist (charset '(kana han cjk-misc bopomofo chinese-gbk))
              (set-fontset-font t charset (font-spec :name font :size 12)))
            (throw 'end t)))
  )

;;; ### Font ###
;;; --- 字体命令
(lazy-load-set-keys
 '(
   ("s--" . text-scale-decrease)        ;减小字体大小
   ("s-=" . text-scale-increase)        ;增大字体大小
   ))

(provide 'init-font)

;;; init-font.el ends here