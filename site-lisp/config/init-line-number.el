;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'display-line-numbers)

;; Line numbers are not displayed when large files are used.
(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 1000)
;; 设置超过120字符显示越界指示器
(setq-default display-fill-column-indicator-column 120)

(dolist (hook (list
               'prog-mode-hook
               'markdown-mode-hook
               ))
  (add-hook hook #'display-line-numbers-mode))

(provide 'init-line-number)
