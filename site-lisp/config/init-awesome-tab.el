;;; init-awesome-tab.el --- Configuration for awesome-tab.el

;; Filename: init-awesome-tab.el
;; Description: Configuration for awesome-tab.el
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-09-25 19:31:19
;; Version: 0.1
;; Last-Updated: 2018-09-25 19:31:19
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-awesome-tab.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
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
;; Configuration for awesome-tab.el
;;

;;; Installation:
;;
;; Put init-awesome-tab.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-awesome-tab)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-awesome-tab RET
;;

;;; Change log:
;;
;; 2018/09/25
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
(require 'awesome-tab)
(require 'all-the-icons)
(require 'init-const)
;; (header-line :inherit 'default :height (face-attribute 'default :height))
;; (set-face-attribute 'mode-line nil  :height 100)
;; (set-face-attribute 'header-line nil  :height 160)
;;; Code:
(awesome-tab-mode t)
(if sys/win32p
    (progn
      (setq awesome-tab-height 110)
      ;; (setq awesome-tab-icon-v-adjust 0)
      (setq awesome-tab-icon-height 0.7)
      ;; (setq awesome-tab-label-fixed-length 14)
      (setq awesome-tab-active-bar-height 22))
  (setq awesome-tab-height 130)
  ;; (setq awesome-tab-icon-v-adjust 0)
  (setq awesome-tab-icon-height 0.8)
  ;; (setq awesome-tab-label-fixed-length 14)
  (setq awesome-tab-active-bar-height 22))


(provide 'init-awesome-tab)

;;; init-awesome-tab.el ends here
