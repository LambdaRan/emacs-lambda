;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-python.el --- Init python

(require 'python)

(lazy-load-local-keys
 '(("C-S-j" . jump-to-import)
   )
 python-mode-map
 "python-extension")

(setq python-shell-completion-native-enable nil)
(setq python-indent-guess-indent-offset-verbose nil)
(setq python-indent-offset 4)
;; default python3
(when (and (executable-find "python3")
           (string= python-shell-interpreter "python"))
  (setq python-shell-interpreter "python3"))

(provide 'init-python)
