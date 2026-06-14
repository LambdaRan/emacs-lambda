;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-indent.el --- Init indent config

;; 全局默认使用空格缩进（Python、JS、TS、YAML 等现代语言均要求空格）
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; 仅对需要 tab 的模式启用
(dolist (hook '(makefile-mode-hook
                makefile-bsdmake-mode-hook
                makefile-gmake-mode-hook
                go-mode-hook             ; Go 语言约定使用 tab
                lua-mode-hook))          ; Lua 使用 tab
  (add-hook hook (lambda () (setq indent-tabs-mode t))))

;; (setq-default c-electric-flag nil)

(defun adjust-languages-indent (n)
  (setq-local c-basic-offset n))

(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'asm-mode-hook
               'emms-tag-editor-mode-hook
               'sh-mode-hook
               'haskell-cabal-mode-hook
               'ruby-mode-hook
               'qml-mode-hook
               'cmake-mode-hook
               'web-mode-hook
               'scss-mode-hook
               'coffee-mode-hook
               'js-mode-hook
               'protobuf-mode-hook
               'php-mode-hook
               'csharp-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (adjust-languages-indent 4)
                     )))

(provide 'init-indent)

;;; init-indent.el ends here
