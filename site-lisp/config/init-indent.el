;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-indent.el --- Init indent config

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

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
