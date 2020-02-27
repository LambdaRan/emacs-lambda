;;; init-cnfonts.el --- Config for cnfonts.

;; Filename: init-cnfonts.el
;; Description: Config for cnfonts.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-07-29 16:29:07
;; Version: 0.1
;; Last-Updated: 2019-07-29 16:29:07
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-cnfonts.el
;; Keywords:
;; Compatibility: GNU Emacs 26.2
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
;; Config for cnfonts.
;;

;;; Installation:
;;
;; Put init-cnfonts.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-cnfonts)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-cnfonts RET
;;

;;; Change log:
;;
;; 2019/07/29
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
(add-hook 'org-mode-hook
          '(lambda ()
             (require 'cnfonts)
             (setq fontset-orgtable
                   (create-fontset-from-ascii-font "Monaco 14"))
             (dolist (charset '(han symbol cjk-misc))
               (set-fontset-font fontset-orgtable charset
                                 (font-spec :family "Hiragino Sans GB W3"
                                            :size 16)))
             (set-face-attribute 'org-table nil
                                 :font "Monaco 14"
                                 :fontset fontset-orgtable)))

(provide 'init-cnfonts)

;;; init-cnfonts.el ends here