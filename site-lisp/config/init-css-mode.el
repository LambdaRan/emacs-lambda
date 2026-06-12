;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-css-mode.el --- Init for css-mode

(require 'css-mode)

(dolist (hook (list
               'css-mode-hook))
  (add-hook hook #'(lambda ()
                     (require 'rainbow-mode)
                     (rainbow-mode))))

(provide 'init-css-mode)
