;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-c.el --- Setup for c/cpp mode

(defun c-mode-style-setup ()
  "Set up C/C++ mode."
  (interactive)
  (unless (eq major-mode 'java-mode)

    ;; @see http://stackoverflow.com/questions/3509919/ \
    ;; emacs-c-opening-corresponding-header-file
    (local-set-key (kbd "C-x C-o") 'ff-find-other-file)
    (setq ff-quiet-mode t)
    ;; 找不到同名文件时不创建
    (setq ff-always-try-to-create nil)
    (setq ff-case-fold-search t)
    (setq cc-search-directories '("."
                                  "/usr/include"
                                  "/usr/local/include/*"
                                  "../*/include"
                                  "../*/src"
                                  "$PROJECT/include"
                                  "$PROJECT/src"
                                  "/usr/local/opt/llvm/include/c++/v1"
                                  ))
    ;; make a #define be left-aligned
    (setq c-electric-pound-behavior '(alignleft))

    ;; cpp font lock.
    (modern-c++-font-lock-global-mode t)
    ;; modern-c++-generate-font-lock-keywords
    ;; modern-c++-generate-font-lock-stl-cstdint
    ;; 这两个函数耗时

    ;; google-c-style
    (google-set-c-style)
    (google-make-newline-indent)
    ;; base-style
    ;; (c-set-style "stroustrup")

    ;; 输入标点符号时自动缩进，禁止
    (setq c-electric-flag nil)
    ;; (setq-default c-electric-flag nil)
    ;; 回车自动缩进,默认打开
    ;; (electric-indent-mode -1)

    ;; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Syntactic-Symbols.html#Syntactic-Symbols
    (setq c-syntactic-indentation t)
    (c-set-offset 'access-label -2)

    (subword-mode)
    ))


(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'c-mode-common-hook
               ))
  (add-hook hook
            #'(lambda ()
                (require 'cc-mode)
                ;; (require 'c-eldoc)
                (require 'modern-cpp-font-lock)
                (require 'google-c-style)
                (c-mode-style-setup))))

(add-hook 'c-mode-hook #'(lambda ()
                           (setq-local comment-start "// ")
                           (setq-local comment-end "")
                           ))
(provide 'init-c)

;;; init-c.el ends here
