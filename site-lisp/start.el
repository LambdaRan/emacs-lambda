;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

(setq my-init-file (or load-file-name (buffer-file-name)))
(message "lambda %s" my-init-file)
;; (setq user-emacs-directory (file-name-directory user-init-file))

(require 'init)

(provide 'start)
