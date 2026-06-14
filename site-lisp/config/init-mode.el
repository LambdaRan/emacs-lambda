;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-mode.el --- File mode setup

;;; Code:

;;; ### auto-mode-alist ###
;;; --- 绑定扩展名到特定的模式
(dolist (elt-cons '(
                    ("\\.php\\'" . php-mode)
                    ("\\.vue" . web-mode)
                    ("\\.wxml" . web-mode)
                    ("\\.blade\\.php\\'" . web-mode)
                    ("\\.phtml\\'" . web-mode)
                    ("\\.tpl\\.php\\'" . web-mode)
                    ("\\.jsp\\'" . web-mode)
                    ("\\.as[cp]x\\'" . web-mode)
                    ("\\.erb\\'" . web-mode)
                    ("\\.mustache\\'" . web-mode)
                    ("\\.djhtml\\'" . web-mode)
                    ("\\.html?\\'" . web-mode)
                    ("\\.jsx$" . web-mode)
                    ("\\.css\\'" . css-mode)
                    ("\\.wxss\\'" . css-mode)
                    ("\\.go$" . go-mode)
                    ("\\.rs$" . rust-mode)
                    ("\\.js$" . js-mode)
                    ("\\.wxs$" . js-mode)
                    ("\\.lua$" . lua-mode)
                    ("\\.cpp$" . c++-mode)
                    ("\\.h$" . c++-mode)
                    ("\\.yaml\\'" . yaml-mode)
                    ("\\.yml\\'" . yaml-mode)
                    ("\\.cs\\'" . csharp-mode)
                    ("\\.pro$" . qmake-mode)
                    ("\\.proto$" . protobuf-mode)
                    ))
  (add-to-list 'auto-mode-alist elt-cons))

;;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))
(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;; Mode load.
(autoload 'web-mode "init-web-mode")
(autoload 'php-mode "php-mode")
(autoload 'go-mode "init-golang")
(autoload 'rust-mode "rust-mode")
(autoload 'csharp-mode "csharp-mode")
(autoload 'python-mode "init-python")
(autoload 'lua-mode "init-lua")
(autoload 'yaml-mode "yaml-mode")
(autoload 'protobuf-mode "protobuf-mode")

;;; ### Auto-fill ###
;;; --- 自动换行
(setq default-fill-column 120)          ;默认 120 列就换行
(dolist (hook (list
               'after-text-mode-hook
               'message-mode-hook
               'org-mode-hook
               ))
  (add-hook hook #'(lambda () (auto-fill-mode 1))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (require 'sly-el-indent)
            (sly-el-indent-setup)))

(with-eval-after-load  'ielm
  (require 'elispfl)
  (elispfl-ielm-mode))

(add-hook 'php-mode-hook
          #'(lambda ()
              ;; 关闭对HTML的支持
              (setq php-template-compatibility nil)

              (setq tab-width 4
                    c-basic-offset 4
                    c-hanging-comment-ender-p nil)
              (php-enable-pear-coding-style)
              ))


(provide 'init-mode)

;;; init-mode.el ends here
