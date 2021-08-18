;; -*- coding: utf-8; lexical-binding: t; -*-
;; init-accelerate.el --- Accelerate the start Emacs

;; Filename: init-accelerate.el
;; Description: Accelerate the start Emacs
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2020, Andy Stewart, all rights reserved.
;; Created: 2020-03-22 11:17:30
;; Version: 0.1
;; Last-Updated: 2020-03-22 11:17:30
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-accelerate.el
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
;; Accelerate the start Emacs
;;

;;; Installation:
;;
;; Put init-accelerate.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-accelerate)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-accelerate RET
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
(require 'gcmh)

;;; Code:
(setq frame-inhibit-implied-resize t    ;;; 不要缩放frame.
      initial-major-mode 'fundamental-mode ; ;; 默认用最简单的模式
      package-enable-at-startup nil        ; ;; 不要自动启用package
      package--init-file-ensured t)

;; Improve the performance of rendering long lines.
(setq-default bidi-display-reordering nil)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it there anyway, just in case. This increases
;; memory usage, however!
(setq inhibit-compacting-font-caches t)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Scrolling.html
;; https://www.reddit.com/r/emacs/comments/gaub11/poor_scrolling_performance_in_doom_emacs/fp392eh/

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 64 1024))  ; 64kb

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when sys/windows-p
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster IPC
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless sys/mac-p   (setq command-line-ns-option-alist nil))
(unless sys/linux-p (setq command-line-x-option-alist nil))

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; https://github.com/seagle0128/.emacs.d/blob/master/init.el
;; Speed up startup
(setq auto-mode-case-fold nil)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              "Recover GC values after startup."
              (setq gc-cons-threshold 800000
                    gc-cons-percentage 0.1)))

 ;; Garbage Collector Magic Hack
(setq gcmh-idle-delay 5
      gcmh-high-cons-threshold #x1000000) ; 16MB
(gcmh-mode 1)

(provide 'init-accelerate)

;;; init-accelerate.el ends here