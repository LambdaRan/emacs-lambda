;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-flycheck.el --- Configuration for flycheck


;;; Require

;;; Code:

;; Add flycheck for Rust.
(add-hook 'rust-mode-hook
          #'(lambda ()
              (require 'flycheck)
              (flycheck-mode 1)
              (require 'flycheck-rust)
              (flycheck-rust-setup)
              ))

(with-eval-after-load 'flycheck
  (setq flycheck-check-syntax-automatically '(idle-change save))
  (setq flycheck-emacs-lisp-load-path 'inherit)
  ;; (setq flycheck-indication-mode nil)
  (setq-default flycheck-temp-prefix ".flycheck")

  (defun magnars/adjust-flycheck-automatic-syntax-eagerness ()
    "Adjust how often we check for errors based on if there are any.
This lets us fix any errors as quickly as possible, but in a
clean buffer we're an order of magnitude laxer about checking."
    (setq flycheck-idle-change-delay
          (if flycheck-current-errors 1 3.0)))
  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.
  (make-variable-buffer-local 'flycheck-idle-change-delay)
  (add-hook 'flycheck-after-syntax-check-hook
            'magnars/adjust-flycheck-automatic-syntax-eagerness)
  ;; (setq flycheck-idle-change-delay 2)  
  
  ;; rollback Tag v0.85 没有黑窗口
  ;; (with-eval-after-load 'flycheck
  ;;   (require 'flycheck-posframe)
  ;;   (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))
  )
(provide 'init-flycheck)

;;; init-flycheck.el ends here
