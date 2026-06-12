;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-auto-save.el --- Init for auto-save.el
(require 'auto-save)

;; 不要自动备份，auto-save.el 就挺好用
(setq make-backup-files nil)
(setq auto-save-default nil)

(auto-save-enable)
(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace nil)
(setq auto-save-idle 1)

(provide 'init-auto-save)
