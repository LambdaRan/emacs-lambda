;; -*- coding: utf-8; lexical-binding: t; -*-
;; https://github.com/dgutov/diff-hl

;; (require 'diff-hl)
;; (require 'diff-hl-margin)
;; (global-diff-hl-mode)
;; (diff-hl-flydiff-mode)

;; `diff-hl-diff-goto-hunk'  C-x v =
;; `diff-hl-revert-hunk'     C-x v n
;; `diff-hl-previous-hunk'   C-x v [
;; `diff-hl-next-hunk'       C-x v ]

(require 'init-const)

(unless sys/windows-p
  (dolist (hook (list
                 'prog-mode-hook
                 'conf-mode-hook
                 'protobuf-mode-hook
                 ))
    (add-hook hook #'(lambda ()
                       (require 'diff-hl-margin)
                       (turn-on-diff-hl-mode)))))

(with-eval-after-load 'diff-hl-margin
  ;; right fringe
  (diff-hl-margin-mode)
  (setq diff-hl-side 'right)

  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)
  ;; (add-hook 'dired-mode-hook 'diff-hl-dired-
  )

(provide 'init-diff-hl)
