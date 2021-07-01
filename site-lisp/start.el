;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

(setq user-init-file (or load-file-name (buffer-file-name)))
(message "lambda %s" user-init-file)
;; (setq user-emacs-directory (file-name-directory user-init-file))

(require 'init)

(provide 'start)