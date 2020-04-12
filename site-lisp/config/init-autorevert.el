

;; (setq global-auto-revert-mode t)
;; (add-hook 'c-mode-hook #'turn-on-auto-revert-mode)
(add-hook 'prog-mode-hook
          '(lambda ()
            (custom-set-variables
             ;; '(auto-revert-verbose nil)
             ;; 设置间隔时间
             '(auto-revert-interval 3)) 
            ;; start auto-revert
            (turn-on-auto-revert-mode)
            ))

(provide 'init-autorevert)