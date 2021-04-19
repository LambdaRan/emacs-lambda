
;;; Require

;; https://manateelazycat.github.io/emacs/2018/09/17/indium.html
;; (require 'indium)

(require 'web-mode)
(require 'js)

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OS Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (featurep 'cocoa)
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; disable automatic insertion of double quotes, not easy to use if cursor in string
(setq web-mode-enable-auto-quoting nil)
(setq web-mode-tag-auto-close-style 2) ;; 2 mean auto-close with > and </.
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; js2-mode
(setq-default
 ;; {{ comment indention in modern frontend development
 javascript-indent-level 2
 js-indent-level 2
 css-indent-offset 2
 typescript-indent-level 2
 ;; }}
 js2-strict-trailing-comma-warning nil ; it's encouraged to use trailing comma in ES6
 js2-idle-timer-delay 0.5 ; NOT too big for real time syntax check
 js2-skip-preprocessor-directives t
 js2-strict-inconsistent-return-warning nil ; return <=> return null
 js2-bounce-indent-p t)

