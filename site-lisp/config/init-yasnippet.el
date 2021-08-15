;;; init-yasnippet.el --- Yasnippet configuration

;; Filename: init-yasnippet.el
;; Description: Yasnippet configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 10:29:11
;; Version: 0.1
;; Last-Updated: 2008-10-20 10:29:14
;;           By: Andy Stewart
;; URL:
;; Keywords: yasnippet
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
;; Yasnippet configuration
;;

;;; Installation:
;;
;; Put init-yasnippet.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-yasnippet)
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

;; code

;; my private snippets, should be placed before enabling yasnippet
(setq my-yasnippets (expand-file-name "~/ransysconf/yasnippets"))

(with-eval-after-load 'company
  (require 'yasnippet))

(with-eval-after-load 'yasnippet
  ;; http://stackoverflow.com/questions/7619640/emacs-latex-yasnippet-why-are-newlines-inserted-after-a-snippet
  (setq-default mode-require-final-newline nil)
  
  (add-to-list `yas-snippet-dirs (concat my-emacs-extension-dir "/yasnippet-snippets/snippets"))
  ;; (add-to-list 'yas-snippet-dirs (concat my-emacs-extension-dir "/yasnippet-php-mode"))
  ;; ����Լ���ģ��
  (when (and (file-exists-p my-yasnippets)
             (not (member my-yasnippets yas-snippet-dirs)))
    (add-to-list 'yas-snippet-dirs my-yasnippets))

  ;; Disable yasnippet mode on some mode.
  (dolist (hook (list
                 'term-mode-hook
                 ))
    (add-hook hook #'(lambda () (yas-minor-mode -1))))
  
  (yas-global-mode 1)
  ;; (yas-reload-all)
  )

(defun ran-yas-reload-all ()
  "Reload yasnippets."
  (interactive)
  (yas-reload-all)
  (yas-minor-mode 1))

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
