;;; init-flycheck.el --- Configuration for flycheck

;; Filename: init-flycheck.el
;; Description: Configuration for flycheck
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-07-04 21:35:23
;; Version: 0.3
;; Last-Updated: 2018-07-10 07:58:50
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/init-flycheck.el
;; Keywords:
;; Compatibility: GNU Emacs 27.0.50
;;
;; Features that might be required by this library:
;;
;; `flycheck'
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
;; Configuration for flycheck
;;

;;; Installation:
;;
;; Put init-flycheck.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-flycheck)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-flycheck RET
;;

;;; Change log:
;;
;; 2018/07/10
;;      * Turn off js2 mode warnings
;;
;; 2018/07/05
;;      * Use `posframe' for MacOS, bug has fixed at: https://www.emacswiki.org/emacs/init-startup.el
;;
;; 2018/07/04
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



(defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
  "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
  (setq flycheck-idle-change-delay
        (if flycheck-current-errors 1 3.0)))

(defun ran-init-flycheck()
  "Initialize the flycheck."
  (require 'flycheck)

  ;; OS Config
  (when (featurep 'cocoa)
    ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
    (require 'exec-path-from-shell)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GEM_PATH"))
    (exec-path-from-shell-initialize))

  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  (add-hook 'flycheck-after-syntax-check-hook
            'magnars/adjust-flycheck-automatic-syntax-eagerness)

  ;; (setq flycheck-idle-change-delay 2)
  (setq flycheck-check-syntax-automatically '(idle-change save))

  (setq flycheck-emacs-lisp-load-path 'inherit)
  ;; (setq flycheck-indication-mode nil)
  (setq-default flycheck-temp-prefix ".flycheck")

  ;; 有问题，产生黑窗口
  ;; (with-eval-after-load 'flycheck
  ;;   (require 'flycheck-posframe)
  ;;   (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))
  (flycheck-mode 1))


;; Add flycheck for Rust.
(add-hook 'rust-mode-hook
          #'(lambda ()
              (ran-init-flycheck)
              (require 'flycheck-rust)
              (flycheck-rust-setup)
              ))
;; (with-eval-after-load 'rust-mode
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; 设置flycheck参数，推荐使用本地文件方式 .dir-locals.el
;; https://stackoverflow.com/questions/30949847/configuring-flycheck-to-work-with-c11
;; Open the root directory of your project in Dired with C-x d,
;; and then type M-x add-dir-local-variable RET c++-mode RET flycheck-gcc-language-standard RET "c++11".
;; This will create a .dir-locals.el file in the root directory of your project.
;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;             (setq-default flycheck-gcc-language-standard "c++11")
;;             (setq-default flycheck-clang-language-standard "c++11")
;;             (setq-default flycheck-cppcheck-standards "c++11")
;;             (setq include-path '(;;"/usr/local/opt/llvm/include"
;;                                  "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include"))
;;             (setq-default flycheck-clang-include-path include-path)
;;             (setq-default flycheck-gcc-include-path include-path)
;;             (setq-default flycheck-cppcheck-include-path include-path)
;;             ))


(provide 'init-flycheck)

;;; init-flycheck.el ends here
