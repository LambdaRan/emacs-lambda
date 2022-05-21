;;; init-corfu-tabnine.el --- Configuration for tabnine-capf -*-lexical-binding: t-*-



;;; Code:

;; The free version of TabNine is good enough,
;; and below code is recommended that TabNine not always
;; prompt me to purchase a paid version in a large project.
(defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
  (let ((company-message-func (ad-get-arg 0)))
    (when (and company-message-func
               (stringp (funcall company-message-func)))
      (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
        ad-do-it))))

(with-eval-after-load 'corfu

;; (require 'tabnine-capf)
;; (require 'company-ctags) 

;; (setq tabnine-capf-always-trigger nil) ; 不要一直触发
;; config company-ctags
;; (setq company-ctags-ignore-case t)  ; I use company-ctags instead
;; (company-ctags-auto-setup)

(require 'company-tabnine)
  
;; 组合不同的补全后端
(defun Corfu-multi-backends ()
  (setq-local completion-category-defaults nil)
  (setq-local completion-at-point-functions
              (list 
               (cape-capf-buster
                (cape-super-capf
                 (cape-company-to-capf #'company-tabnine)
                 ;; #'tabnine-completion-at-point
                 ;; (cape-company-to-capf #'company-ctags)
                 #'cape-dabbrev
                 )
                'equal)               
               )))

;; TabNine Start
(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'python-mode-hook
               'go-mode-hook
               'cmake-mode-hook
               'php-mode-hook
               'web-mode-hook
               'rust-mode-hook
               'lua-mode-hook
               'llvm-mode-hook
               'conf-toml-mode-hook
               ))
  (add-hook hook (lambda ()
                   (message "Model[%s] start tabnine" hook)
                   (Corfu-multi-backends)
                   )))
) ;; End with-eval-after-load

(provide 'init-corfu-tabnine)
