(require 'multi-vterm)                   ;多标签SHELL

;;; Code:

(setq vterm-kill-buffer-on-exit t)
(setq multi-vterm-program "/bin/zsh")

(defun ran-vterm-open-in-right-or-below-window (&optional side)
  "Open vterm buffer at right or below windown."
  (interactive)
  (or side (setq side 'right))
  (save-excursion
    (let (other-window new-vterm-buffer)
      (when (one-window-p t)
        (setq other-window (split-window (selected-window) nil side)))
      (other-window 1)
      (or other-window (setq other-window (selected-window)))
      (setq new-vterm-buffer (multi-vterm))
      (set-window-buffer other-window new-vterm-buffer))))

(defun ran-vterm-open-in-right-window ()
  (interactive)
  (ran-vterm-open-in-right-or-below-window))

(defun ran-vterm-open-in-below-window ()
  (interactive)
  (ran-vterm-open-in-right-or-below-window 'below))

(provide 'init-multi-vterm)
