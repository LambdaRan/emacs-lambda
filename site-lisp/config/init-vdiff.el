;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'vdiff)

;; Don't use folding in vdiff buffers if non-nil.
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

(provide 'init-vdiff)