;;; init-company-mode.el --- Company-mode configuration

;; Filename: init-company-mode.el
;; Description: Company-mode configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:56:57
;; Version: 1.1
;; Last-Updated: 2018-09-11 01:00:24
;;           By: Andy Stewart
;; URL:
;; Keywords: company-mode
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
;; Company-mode configuration
;;

;;; Installation:
;;
;; Put init-company-mode.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-company-mode)
;;

;;; Change log:
;;
;; 2018/09/11
;;      * Split lsp configuration to `init-lsp.el'.
;;
;; 2018/07/24
;;      * Add command to install python completion backend.
;;
;; 2018/07/23
;;      * Add `company-elisp' backend when load emacs-lisp mode.
;;
;; 2018/07/16
;;      * Don't downcase completion result from dabbrev.
;;
;; 2018/07/12
;;      * Customize dabbrev backend, to make company can completion any words in all buffer like `dabbrev-expand'.
;;      * Default add `company-files' backend.
;;
;; 2018/07/07
;;      * Add `company-css' into `company-backends'.
;;
;; 2018/07/06
;;      * Fix ruby mode load error.
;;      * Fix python mode load error.
;;      * Use `exec-path-from-shell' avoid LSP can't found server bin path.
;;
;; 2018/07/05
;;      * Config company and company-lsp fronted.
;;      * Make company works with posframe.
;;      * Add LSP mode support.
;;      * Add support for python and rubyl
;;
;; 2008/10/20
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
(defun company-tabnine-sort-by-detail (candidates)
  "Sort tabnine response by detail value"
  (sort candidates
        (lambda (c1 c2)
          (let ((d1 (get-text-property 0 'detail c1))
                (d2 (get-text-property 0 'detail c2)))
            ;; 默认忽略type
            (or d1 (setq d1 "1%"))
            (or d2 (setq d2 "1%"))
            (>= (string-to-number d1) (string-to-number d2))))))

(defun company-sort-by-tabnine-and-ctags (candidates)
  "sort company backends response"
  (when (or (functionp company-backend)
            (not (and (listp company-backend)
                      (memq 'company-tabnine company-backend))))
    candidates)
  (let (candidates-tabnine candidates-ctags candidates-yas)
    (setq candidates-max-length (min (length candidates) 20))
    (dolist (candidate candidates)
      (setq backend-property (get-text-property 0 'company-backend candidate))
      (cond
        ((eq backend-property 'company-ctags)
         (push candidate candidates-ctags))
        ((eq backend-property 'company-yasnippet)
         (push candidate candidates-yas))
        (t (push candidate candidates-tabnine))))
    ;; (setq candidates-tabnine (nreverse candidates-tabnine))
    (setq candidates-tabnine (company-tabnine-sort-by-detail candidates-tabnine))
    (setq candidates-ctags (nreverse candidates-ctags))
    (setq candidates-yas (nreverse candidates-yas))
    (setq candidates (nconc (seq-take candidates-tabnine 5)
                            (seq-take candidates-ctags 3)
                            (seq-take candidates-yas 2)))
    (setq candidates-other (nconc (seq-drop candidates-tabnine 5)
                                  (seq-drop candidates-ctags 3)
                                  (seq-drop candidates-yas 2)))
    (let ((len (length candidates)))
      (when (< len candidates-max-length)
        (setq candidates (nconc candidates (seq-take candidates-other (- candidates-max-length len))))))

    candidates))

(add-hook 'prog-mode-hook
          '(lambda ()
            (require 'lazy-load)
            (require 'company)
            (require 'company-yasnippet)
            (require 'company-dabbrev)
            (require 'company-files)
            (require 'company-tng)

            ;; Config for company mode.
            ;; Trigger completion immediately.
            (setq company-idle-delay 0)
            ;; 补全的最小前缀长度
            (setq company-minimum-prefix-length 2) ; pop up a completion menu by tapping a character
            ;; Number the candidates (use M-1, M-2 etc to select completions).
            (setq company-show-numbers t)   ; do not display numbers on the left
            (setq company-require-match nil) ; allow input string that do not match candidate words

            ;; Don't downcase the returned candidates.
            (setq company-dabbrev-downcase nil
             ;; make previous/next selection in the popup cycles
             company-selection-wrap-around t
             company-dabbrev-ignore-case t
             ;; @see https://github.com/company-mode/company-mode/issues/146
             company-tooltip-align-annotations t)

            ;; (add-to-list 'company-transformers 'company-sort-by-backend-importance)
            (add-to-list 'company-transformers 'company-sort-by-tabnine-and-ctags t)
            ;; NOT to load company-mode for certain major modes.
            ;; Ironic that I suggested this feature but I totally forgot it
            ;; until two years later.
            ;; https://github.com/company-mode/company-mode/issues/29
            (setq company-global-modes
             '(not
               eshell-mode comint-mode erc-mode gud-mode rcirc-mode text-mode
               minibuffer-inactive-mode))

            ;; config company-ctags
            (setq company-ctags-ignore-case t)  ; I use company-ctags instead

            ;; Customize company backends.
            (setq company-backends (delete 'company-xcode company-backends))
            (setq company-backends (delete 'company-bbdb company-backends))
            (setq company-backends (delete 'company-eclim company-backends))
            (setq company-backends (delete 'company-gtags company-backends))
            ;; (setq company-backends (delete 'company-etags company-backends))
            (setq company-backends (delete 'company-oddmuse company-backends))
            (setq company-backends (delete 'company-cmake company-backends))
            ;; (dolist (backend '(company-xcode company-bbdb company-eclim company-gtags company-etags company-oddmuse company-cmake))
            ;;   (setq company-backends (delete backend company-backends)))
            (add-to-list 'company-backends 'company-files)

            ;; Use the tab-and-go frontend.
            ;; Allows TAB to select and complete at the same time.
            (company-tng-configure-default)
            (setq company-frontends
             '(company-tng-frontend
               company-pseudo-tooltip-frontend
               company-echo-metadata-frontend))

            ;; Enable global.
            (global-company-mode)

            ;; Add `company-elisp' backend for elisp.
            (add-hook 'emacs-lisp-mode-hook
             '(lambda ()
               (require 'company-elisp)
               (add-to-list 'company-backends 'company-elisp)))

             ;; Key settings.
             (lazy-load-unset-keys
              '("TAB")
              company-mode-map)         ;unset default keys

             (lazy-load-unset-keys
              '("M-p" "M-n" "C-m")
              company-active-map)

             (lazy-load-set-keys
              '(
                ("TAB" . company-complete-selection)
                ;; ("M-h" . company-complete-selection)
                ("M-h" . company-complete-common)
                ;; ("M-H" . company-complete-common)
                ("M-w" . company-show-location)
                ("M-s" . company-search-candidates)
                ("M-S" . company-filter-candidates)
                ("M-n" . company-select-next)
                ("M-p" . company-select-previous)
                ("M-i" . yas-expand)
                ("RET" . company-complete-selection)
                )
              company-active-map)

             ;; Add yasnippet support for all company backends.
             (defvar company-mode/enable-yas t
               "Enable yasnippet for all backends.")

             (defun company-mode/backend-with-yas (backend)
               (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
                   backend
                 (append (if (consp backend) backend (list backend))
                         '(:with company-yasnippet))))

             (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
             ))

(provide 'init-company-mode)

;;; init-company-mode.el ends here
