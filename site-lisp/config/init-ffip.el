
(require 'init-const)
(require 'find-file-in-project)

(when sys/mac-cocoa-p
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; config
;; use fd instread find
(setq ffip-use-rust-fd t)
;; "Don 't show search results from '.*ignore' files."

(setq ffip-prefer-ido-mode nil)

;; (setq ffip-project-root "~/projs/PROJECT_DIR")
;; ffip-project-root-function

;; find-file-in-project
;; ffip-lisp-find-file-in-project
;; ffip-show-diff
;; ffip-split-window-horizontally
;; ffip-split-window-vertically
;; ffip-insert-file

(provide 'init-ffip)
