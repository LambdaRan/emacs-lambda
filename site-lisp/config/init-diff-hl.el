
;; https://github.com/dgutov/diff-hl

;; (require 'diff-hl)
(require 'diff-hl-margin)
;; (global-diff-hl-mode)
;; (diff-hl-flydiff-mode)

;; `diff-hl-diff-goto-hunk'  C-x v =
;; `diff-hl-revert-hunk'     C-x v n
;; `diff-hl-previous-hunk'   C-x v [
;; `diff-hl-next-hunk'       C-x v ]


;; (unless (window-system) (diff-hl-margin-mode))
;; right fringe
(diff-hl-margin-mode)
(setq diff-hl-margin-side 'right)

(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)

(dolist (hook (list
               'prog-mode-hook
               ;; conf
               'conf-mode-hook
               ;; 'conf-unix-mode-hook
               ;; 'conf-windows-mode-hook
               ;; 'conf-javaprop-mode-hook
               ;; 'conf-space-mode-hook
               ;; 'conf-colon-mode-hook
               ;; 'conf-ppd-mode-hook
               ;; 'conf-toml-mode-hook
               ;; 'conf-windows-mode-hook
               ;; 'conf-xdefaults-mode-hook
               ))
  (add-hook hook 'turn-on-diff-hl-mode))

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)
;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)




(provide 'init-diff-hl)
