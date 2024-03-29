;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-company-tabnine.el --- Configure for TabNine

;; Filename: init-company-tabnine.el
;; Description: Configure for TabNine
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2020, Andy Stewart, all rights reserved.
;; Created: 2020-03-28 15:54:37
;; Version: 0.1
;; Last-Updated: 2020-03-28 15:54:37
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-company-tabnine.el
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
;; Configure for TabNine
;;

;;; Installation:
;;
;; Put init-company-tabnine.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-company-tabnine)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-company-tabnine RET
;;

;;; Change log:
;;
;; 2020/03/28
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

;; The free version of TabNine is good enough,
;; and below code is recommended that TabNine not always
;; prompt me to purchase a paid version in a large project.
(defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
  (let ((company-message-func (ad-get-arg 0)))
    (when (and company-message-func
               (stringp (funcall company-message-func)))
      (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
        ad-do-it))))

(with-eval-after-load 'company
  (require 'company-tabnine)
  (require 'company-dabbrev)
  (setq company-tabnine-always-trigger nil) ; 不要一直触发

  ;; TabNine
  (dolist (mode (list
                 'c-mode-common
                 'c-mode
                 'emacs-lisp-mode
                 'lisp-interaction-mode
                 'lisp-mode
                 'makefile-gmake-mode
                 'python-mode
                 'go-mode
                 'cmake-mode
                 'php-mode
                 'web-mode
                 'rust-mode
                 'lua-mode
                 'llvm-mode
                 'conf-toml-mode
                 ))
    (with-eval-after-load mode
      (unless (catch 'found
                (dolist (b company-backends)
                  (cond
                    ((equal b 'company-tabnine)
                     (throw 'found t))
                    ((and (listp b) (member 'company-tabnine b))
                     (throw 'found t))
                    (t nil))))
        (add-to-list 'company-backends '(company-tabnine
                                         :separate company-dabbrev
                                         )))))
  )

(provide 'init-company-tabnine)

;;; init-company-tabnine.el ends here
