;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-golang.el --- Extensions for go lang mode

(require 'go-mode)

(defun go-run-buffer ()
  "Run current Go file using `compile' (non-blocking)."
  (interactive)
  (compile (concat "go run " (shell-quote-argument (buffer-file-name)))))

(lazy-load-unset-keys
 '("C-k" "M-o")
 go-mode-map)

(lazy-load-set-keys
 '(
   ("C-c C-c" . go-run-buffer)
   ("C-c C-f" . gofmt)
   ;; ("C-c C-d" . godoc)
   )
 go-mode-map)

(provide 'init-golang)
