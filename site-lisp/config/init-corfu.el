

;;; Require

;;; Code:
(add-hook 'prog-mode-hook
          #'(lambda ()
              (require 'corfu)
              (global-corfu-mode)))

(with-eval-after-load 'corfu
(require 'corfu)
(require 'cape)
(require 'orderless)

(setq corfu-auto t)
(setq corfu-cycle t)
(setq corfu-auto-prefix 2)
(setq corfu-auto-delay 0)
(setq corfu-scroll-margin 5)
;; (setq corfu-quit-at-boundary t)
(setq corfu-quit-no-match t)
(setq corfu-excluded-modes '(shell-mode eshell-mode comint-mode erc-mode gud-mode rcirc-mode text-mode minibuffer-inactive-mode))  

;; 默认补全后端
(add-to-list 'completion-at-point-functions #'cape-symbol)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)  

(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles basic partial-completion))))
(setq completion-category-defaults nil)
;; 修改默认按键
(lazy-load-unset-keys
 '("M-p" "M-n" "M-h" "M-g" "C-m" "C-n" "C-p")
 corfu-map)
(lazy-load-set-keys
 '(
   ("RET" . corfu-complete)
   ("C-n" . corfu-next)
   ("C-p" . corfu-previous)
   ("M-w" . corfu-info-location)
   )
 corfu-map)
) ;; End with-eval-after-load

(provide 'init-corfu)
