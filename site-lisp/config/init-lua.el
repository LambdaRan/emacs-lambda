;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-lua.el --- Init lua

(require 'lua-mode)

(setq lua-indent-level 4)
(add-hook 'lua-mode-hook (lambda () (setq indent-tabs-mode t)))

(provide 'init-lua)
