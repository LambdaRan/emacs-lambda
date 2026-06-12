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

;; ;; Try to compile using the appropriate version of Python for
;; ;; the file.
;; (setq-local flycheck-python-pycompile-executable executable)
;; ;; We might be running inside a virtualenv, in which case the
;; ;; modules won't be available. But calling the executables
;; ;; directly will work.
;; (setq-local flycheck-python-pylint-executable "pylint")
;; (setq-local flycheck-python-flake8-executable "flake8")

(provide 'init-python)
