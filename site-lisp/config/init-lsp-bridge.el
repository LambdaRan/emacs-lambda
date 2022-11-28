;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Require
(require 'lsp-bridge)
(global-lsp-bridge-mode)

;; (setq lsp-bridge-get-single-lang-server-by-project
;;       (lambda (project-path filepath)
;;         ;; If typescript file include deno.land url, then use Deno LSP server.
;;         (save-excursion
;;           (when (string-equal (file-name-extension filepath) "ts")
;;             (dolist (buf (buffer-list))
;;               (when (string-equal (buffer-file-name buf) filepath)
;;                 (with-current-buffer buf
;;                   (goto-char (point-min))
;;                   (when (search-forward-regexp (regexp-quote "from \"https://deno.land") nil t)
;;                     (return "deno")))))))))

(setq lsp-bridge-enable-diagnostics nil)
(setq acm-enable-doc nil)
;; (setq acm-enable-tabnine t)
;; (setq acm-enable-quick-access t)
(setq acm-backend-lsp-candidate-min-length 2)

;; 打开日志
;; (setq lsp-bridge-enable-log t)
(provide 'init-lsp-bridge)