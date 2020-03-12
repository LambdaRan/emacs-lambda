;;; init-qt.el --- Setup for qt mode

;; Filename: init-qt.el
;; Description: Setup for qt mode
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2017, Andy Stewart, all rights reserved.
;; Created: 2017-02-06 21:29:16
;; Version: 0.1
;; Last-Updated: 2017-02-06 21:29:16
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-qt.el
;; Keywords:
;; Compatibility: GNU Emacs 25.0.50.1
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
;; Setup for qt mode
;;

;;; Installation:
;;
;; Put init-qt.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-qt)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-qt RET
;;

;;; Change log:
;;
;; 2017/02/06
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

;;; Code:


(defun c-mode-style-setup ()
  (interactive)
  "Set up C/C++ mode"
  (unless (eq major-mode 'java-mode)

    ;; @see http://stackoverflow.com/questions/3509919/ \
    ;; emacs-c-opening-corresponding-header-file
    (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
    ;; 找不到同名文件时不创建
    (setq ff-always-try-to-create nil)
    (setq cc-search-directories '("."
                                  "/usr/include"
                                  "/usr/local/include/*"
                                  "../*/include"
                                  "../*/src"
                                  "$PROJECT/include"
                                  "$PROJECT/src"
                                  "/usr/local/opt/llvm/include/c++/v1"
                                  ))
    ;; make a #define be left-aligned
    (setq c-electric-pound-behavior (quote (alignleft)))

    ;; cpp font lock.
    (modern-c++-font-lock-global-mode t)

    ;; google-c-style
    (google-set-c-style)
    (google-make-newline-indent)
    ;; base-style
    ;; (c-set-style "stroustrup")

    ;; 输入标点符号时自动缩进，禁止
    (setq c-electric-flag nil)
    ;; (setq-default c-electric-flag nil)
    ;; 回车自动缩进,默认打开
    ;; (electric-indent-mode -1)

    ;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Syntactic-Symbols.html#Syntactic-Symbols
    (setq c-syntactic-indentation t)
    (c-set-offset 'access-label -2)

    (subword-mode)
    ))


(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'c-mode-common-hook
               ))
  (add-hook
   hook
   '(lambda ()
     (require 'cc-mode)
     ;; (require 'c-eldoc)
     (require 'modern-cpp-font-lock)
     (require 'google-c-style)

     (c-mode-style-setup)
     )))

(provide 'init-c)

;;; init-qt.el ends here
