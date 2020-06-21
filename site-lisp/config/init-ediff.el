;; -*- coding: utf-8; lexical-binding: t; -*-



;; https://github.com/abo-abo/oremacs/blob/github/modes/ora-ediff.el

(require 'ediff)
(require 'diff-mode)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-merge-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w --text")


(defun ora-ediff-prepare-buffer ()
  (when (memq major-mode '(org-mode emacs-lisp-mode))
    (require 'outline)
    (outline-show-all))
  ;; (when (> (max-line-width) 150)
  ;;   (visual-line-mode))
  )

(add-hook 'ediff-prepare-buffer-hook 'ora-ediff-prepare-buffer)
(add-hook 'ediff-quit-hook 'winner-undo)

(defun ora-ediff-jk ()
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-keymap-setup-hook #'ora-ediff-jk)



(provide 'init-ediff)
