
(require 'init-const)

(unless sys/windows-p

  (require 'multi-vterm)                   ;多标签SHELL

  (setq vterm-kill-buffer-on-exit t)
  (setq multi-vterm-program "/bin/zsh")

  (defun ran-vterm-open-in-right-or-below-window (&optional side)
    "Open vterm buffer at right or below windown."
    (interactive)
    (or side (setq side 'right))
    (save-excursion
      (let (other-window-v
            new-vterm-buffer
            (cur-file-name buffer-file-name))
        (when (and (not cur-file-name)
                   (eq major-mode 'dired-mode))
          (setq cur-file-name (dired-get-file-for-visit)))
        (setq origin-dir (and cur-file-name (file-name-directory cur-file-name)))
        (when (one-window-p t)
          (setq other-window-v (split-window (selected-window) nil side)))
        (other-window 1)
        (unless other-window-v
          (setq other-window-v (selected-window))
          (when (and origin-dir (file-exists-p origin-dir))
            (setq default-directory origin-dir)))
        (setq new-vterm-buffer (multi-vterm))
        (set-window-buffer other-window-v new-vterm-buffer))))

  (defun ran-vterm-open-in-right-window ()
    (interactive)
    (ran-vterm-open-in-right-or-below-window))

  (defun ran-vterm-open-in-below-window ()
    (interactive)
    (ran-vterm-open-in-right-or-below-window 'below)))

(provide 'init-multi-vterm)
