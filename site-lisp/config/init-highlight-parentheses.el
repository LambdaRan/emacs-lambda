;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'highlight-parentheses)

(setq hl-paren-colors '("firebrick1" "DarkOrchid3" "DarkGoldenrod2" "DodgerBlue3")
      hl-paren-highlight-adjacent t)

(highlight-parentheses-mode t)

(provide 'init-highlight-parentheses)