;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-golang.el --- Extensions for go-ts-mode

(require 'go-ts-mode)

(defun go-run-buffer ()
  "Run current Go file using `compile' (non-blocking)."
  (interactive)
  (compile (concat "go run " (shell-quote-argument (buffer-file-name)))))

(defun go-fmt-buffer ()
  "Format current Go file using goimports or go fmt."
  (interactive)
  (let ((file (buffer-file-name)))
    (when file
      (save-buffer)
      (if (executable-find "goimports")
          (compile (concat "goimports -w " (shell-quote-argument file)))
        (compile (concat "go fmt " (shell-quote-argument file)))))))

(lazy-load-unset-keys
 '("C-k" "M-o")
 go-ts-mode-map)

(lazy-load-set-keys
 '(("C-c C-c" . go-run-buffer)
   ("C-c C-f" . go-fmt-buffer))
 go-ts-mode-map)

(provide 'init-golang)
