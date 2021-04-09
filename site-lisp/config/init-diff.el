
;; https://github.com/abo-abo/oremacs/blob/github/modes/ora-ediff.el

(require 'ediff)
(require 'diff-mode)
(require 'vdiff)

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


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; vdiff

;; don't use folding in vdiff buffers if non-nil.
;; (setq vdiff-disable-folding t)

;; Default syntax table class code to use for identifying "words" in
;; `vdiff-refine-this-change'. Some useful options are
;;
;; "w"   (default) words
;; "w_"  symbols (words plus symbol constituents)
;;
;; For more information see
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
(setq vdiff-default-refinement-syntax-code "w_")

(define-key vdiff-mode-map (kbd "C-x v") vdiff-mode-prefix-map)

;; ### vdiff ###
;; (lazy-load-global-keys
;;  '(
;;    ("C-x v b" . vdiff-buffers)
;;    ("C-x v f" . vdiff-files)
;;    ;; ("C-x v h" . vdiff-hydra/body)
;;    )
;;  "vdiff")
(lazy-load-set-keys
 '(
   ("C-x v b" . vdiff-buffers)
   ("C-x v f" . vdiff-files)
   ))

(provide 'init-diff)
