;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-markdown-mode.el --- Configure for markdown mode.

;; Filename: init-markdown-mode.el
;; Description: Configure for markdown mode.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-09-22 08:25:25
;; Version: 0.1
;; Last-Updated: 2019-09-22 08:25:25
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-markdown-mode.el
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
;; Configure for markdown mode.
;;

;;; Installation:
;;
;; Put init-markdown-mode.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-markdown-mode)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-markdown-mode RET
;;

;;; Change log:
;;
;; 2019/09/22
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
(require 'markdown-mode)
(require 'grip-mode)
;;; Code:

;; https://emacs-china.org/t/markdown-org-grip-mode/10262/48
;; Preview via `grip'
;; Make a keybinding: `C-c C-c g'
(define-key markdown-mode-command-map (kbd "g") #'grip-mode)

;; When nil, update the preview after file saves only, instead of also
;; after every text change
(setq grip-update-after-change nil)

(require 'auth-source)
(let ((credential (auth-source-user-and-password "api.github.com")))
  (setq grip-github-user (car credential)
        grip-github-password (cadr credential)))

;; Or start grip when opening a markdown/org buffer
;; (add-hook 'markdown-mode-hook #'grip-mode)
;; (add-hook 'org-mode-hook #'grip-mode)

(provide 'init-markdown-mode)
;;; init-markdown-mode.el ends here
