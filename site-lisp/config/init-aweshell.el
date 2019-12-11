
;;; Require
(require 'aweshell)

;;; Code:

;; 自定义 eshll prompt
(defun epe-theme-ran()
  "my eshell-prompt theme "
  (setq eshell-prompt-regexp "^[^#\nλ]* λ[#]* ")
  (concat
    (epe-colorize-with-face (concat (eshell/basename (eshell/pwd))) 'epe-dir-face)
    (epe-colorize-with-face " λ" 'epe-symbol-face)
    (epe-colorize-with-face (if (= (user-uid) 0) "#" "") 'epe-sudo-symbol-face)
    " "))

(with-eval-after-load "esh-opt"
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-ran))

;; epe-theme-ran
;; epe-theme-lambda
;; epe-theme-dakrone
;; epe-theme-pipeline

;; (require 'eshell-up)
;; (defalias 'eshell/up 'eshell-up)
;; (defalias 'eshell/up-peek 'eshell-up-peek)

(provide 'init-aweshell)