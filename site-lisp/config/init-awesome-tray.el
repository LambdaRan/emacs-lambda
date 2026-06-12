;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-awesome-tray.el --- Configuration for awesome tray

(require 'init-const)
(require 'awesome-tray)

(awesome-tray-mode 1)
(setq awesome-tray-buffer-name-buffer-changed t)
(setq awesome-tray-active-modules '("location" "parent-dir" "mode-name" "git" "date"))
(when sys/windows-p
  (defun awesome-tray-encoding-info ()
    (format "%s" buffer-file-coding-system))
  (add-to-list 'awesome-tray-module-alist '("encoding" . (awesome-tray-encoding-info awesome-tray-module-git-face)))
  (setq awesome-tray-active-modules '("location" "parent-dir" "mode-name" "encoding" "date"))
)

(provide 'init-awesome-tray)
