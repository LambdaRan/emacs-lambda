;;; init-python.el --- Init python

;; Filename: init-python.el
;; Description: Init python
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, Andy Stewart, all rights reserved.
;; Created: 2014-01-04 11:10:45
;; Version: 0.1
;; Last-Updated: 2014-01-04 11:10:45
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-python.el
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
;; Init python
;;

;;; Installation:
;;
;; Put init-python.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-python)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-python RET
;;

;;; Change log:
;;
;; 2014/01/04
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
(require 'python)

;;; Code:

(lazy-load-local-keys
 '(
   ("C-S-j" . jump-to-import)
   )
 python-mode-map
 "python-extension")

;; Disable readline based native completion
(setq python-shell-completion-native-enable nil)

(setq python-indent-guess-indent-offset-verbose nil)
(setq python-indent-offset 4)
;; default python3
(when (and (executable-find "python3")
           (string= python-shell-interpreter "python"))
  (setq python-shell-interpreter "python3"))

;; ;; Try to compile using the appropriate version of Python for
;; ;; the file.
;; (setq-local flycheck-python-pycompile-executable executable)
;; ;; We might be running inside a virtualenv, in which case the
;; ;; modules won't be available. But calling the executables
;; ;; directly will work.
;; (setq-local flycheck-python-pylint-executable "pylint")
;; (setq-local flycheck-python-flake8-executable "flake8")

(provide 'init-python)

;;; init-python.el ends here
