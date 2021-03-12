(require 'multi-vterm)                   ;多标签SHELL

;;; Code:

(setq vterm-kill-buffer-on-exit t)
(setq multi-vterm-program "/bin/zsh")

(provide 'init-multi-vterm)
