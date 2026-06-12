;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'lazy-load)

(one-key-create-menu
 "DIRECTORY"
 '(
   (("h" . "Home") . (lambda () (interactive) (dired-x-find-file "~/")))
   (("p" . "Emacs Package") . (lambda () (interactive) (dired-x-find-file my-emacs-root-dir)))
   (("l" . "Emacs-lambda") . (lambda () (interactive) (dired-x-find-file my-emacs-config-dir)))
   (("m" . "Lambda code") . (lambda () (interactive) (dired-x-find-file "~/lambda")))

   (("c" . "win c") . (lambda () (interactive) (dired-x-find-file "c:\\")))
   (("d" . "win d") . (lambda () (interactive) (dired-x-find-file "d:\\")))
   (("e" . "win e") . (lambda () (interactive) (dired-x-find-file "e:\\")))
   (("f" . "win f") . (lambda () (interactive) (dired-x-find-file "f:\\")))
   )
 t)

(one-key-create-menu
 "UI"
 '(
   (("t" . "Tool-Bar") . tool-bar-mode)
   (("m" . "Menu-Bar") . menu-bar-mode)
   (("c" . "Scroll-Bar") . scroll-bar-mode))
 t)

(lazy-load-set-keys
 '(
   ("C-c a" . one-key-menu-directory)       ;目录打开菜单
   ))

(provide 'init-one-key)
