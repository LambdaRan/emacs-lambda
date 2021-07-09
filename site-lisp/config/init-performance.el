;;; init-performance.el --- Configure to improve performance

;; Filename: init-performance.el
;; Description: Configure to improve performance
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2019, Andy Stewart, all rights reserved.
;; Created: 2019-02-23 15:42:59
;; Version: 0.1
;; Last-Updated: 2019-02-23 15:42:59
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-performance.el
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
;; Configure to improve performance
;;

;;; Installation:
;;
;; Put init-performance.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-performance)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-performance RET
;;

;;; Change log:
;;
;; 2019/02/23
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
;;; Code:

;; https://github.com/seagle0128/.emacs.d/blob/master/init.el
;; Speed up startup
(defvar centaur-gc-cons-threshold (if (display-graphic-p) 64000000 1600000)
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar centaur-gc-cons-upper-limit (if (display-graphic-p) 512000000 128000000)
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar centaur-gc-timer (run-with-idle-timer 10 t #'garbage-collect)
  "Run garbarge collection when idle 10s.")

(defvar default-file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist nil)
(setq gc-cons-threshold centaur-gc-cons-upper-limit
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after startup."
            (setq file-name-handler-alist default-file-name-handler-alist
                  gc-cons-threshold centaur-gc-cons-threshold
                  gc-cons-percentage 0.1)

            ;; GC automatically while unfocusing the frame
            ;; `focus-out-hook' is obsolete since 27.1
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                  (lambda ()
                    (unless (frame-focus-state)
                      (garbage-collect))))
              (add-hook 'focus-out-hook 'garbage-collect))

            ;; Avoid GCs while using `ivy'/`counsel'/`swiper' and `helm', etc.
            ;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
            (add-hook 'minibuffer-setup-hook
                      (lambda ()
                        "Enlarge gc cons threshold while entering minibuffer."
                        (setq gc-cons-threshold centaur-gc-cons-upper-limit)))
            (add-hook 'minibuffer-exit-hook
                      (lambda ()
                        "Recover gc cons threshold while exiting minibuffer."
                        (setq gc-cons-threshold centaur-gc-cons-threshold)))))

(provide 'init-performance)

;;; init-performance.el ends here
