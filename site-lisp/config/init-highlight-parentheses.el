;; -*- coding: utf-8; lexical-binding: t; -*-

(add-hook 'prog-mode-hook
          #'(lambda ()
              (require 'highlight-parentheses)
              (highlight-parentheses-mode t)))

(with-eval-after-load 'highlight-parentheses
(setq hl-paren-colors '("firebrick1" "DarkOrchid3" "DarkGoldenrod2" "DodgerBlue3")
      hl-paren-highlight-adjacent t)
)

(provide 'init-highlight-parentheses)
