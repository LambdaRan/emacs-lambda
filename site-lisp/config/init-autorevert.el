

;; (setq global-auto-revert-mode t)
;; (add-hook 'c-mode-hook #'turn-on-auto-revert-mode)
(add-hook 'prog-mode-hook
          '(lambda ()
            ;; (custom-set-variables
             ;; '(auto-revert-verbose nil)
             ;; '(auto-revert-interval 3)) ; 间隔
            ;; start auto-revert
            (turn-on-auto-revert-mode)
            ))

(provide 'init-autorevert)