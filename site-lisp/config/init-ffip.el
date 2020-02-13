
(require 'find-file-in-project)
(require 'lazy-load)


(when (featurep 'cocoa)
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; config
;; use fd instread find
(setq ffip-use-rust-fd t)
;; "Don 't show search results from '.*ignore' files."

;; (setq ffip-project-root "~/projs/PROJECT_DIR")
;; ffip-project-root-function

;; find-file-in-project
;; ffip-lisp-find-file-in-project
;; ffip-show-diff
;; ffip-split-window-horizontally
;; ffip-split-window-vertically
;; ffip-insert-file

(lazy-load-set-keys
 '(
   ("C-c f p" . find-file-in-project-at-point)
   ("C-c f f" . find-file-in-project-by-selected)
   ("C-c f s" . find-file-with-similar-name)
   ("C-c f d" . find-directory-in-project-by-selected)
   ;; ("C-c f c" . find-file-in-current-directory)
   ("C-c f c" . find-file-in-current-directory-by-selected)
   ))

(provide 'init-ffip)
