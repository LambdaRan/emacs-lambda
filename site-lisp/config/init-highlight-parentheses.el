
(require 'highlight-parentheses)

(custom-set-variables
  '(hl-paren-colors
   (quote
    ("firebrick1" "DarkOrchid3" "DarkGoldenrod2" "DodgerBlue3")))
 '(hl-paren-highlight-adjacent t))

(highlight-parentheses-mode t)

(provide 'init-highlight-parentheses)