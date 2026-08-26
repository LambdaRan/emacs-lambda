;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'one-key)
(require 'init-const)                   ; my-emacs-root-dir / my-emacs-config-dir

;; 用 `find-file' 而非 `dired-x-find-file'：后者无 autoload cookie，只有
;; init-dired 会 require dired-x，而本文件经 "C-c a" autoload 独立加载，早期
;; 按键必然 void。二者函数体相同，仅 interactive spec 有别，此处均非交互调用。

(one-key-create-menu
 "DIRECTORY"
 '(
   (("h" . "Home") . (lambda () (interactive) (find-file "~/")))
   (("p" . "Emacs Package") . (lambda () (interactive) (find-file my-emacs-root-dir)))
   (("l" . "Emacs-lambda") . (lambda () (interactive) (find-file my-emacs-config-dir)))
   (("m" . "Lambda code") . (lambda () (interactive) (find-file "~/lambda")))

   (("c" . "win c") . (lambda () (interactive) (find-file "c:\\")))
   (("d" . "win d") . (lambda () (interactive) (find-file "d:\\")))
   (("e" . "win e") . (lambda () (interactive) (find-file "e:\\")))
   (("f" . "win f") . (lambda () (interactive) (find-file "f:\\")))
   )
 t)

(one-key-create-menu
 "UI"
 '(
   (("t" . "Tool-Bar") . tool-bar-mode)
   (("m" . "Menu-Bar") . menu-bar-mode)
   (("c" . "Scroll-Bar") . scroll-bar-mode))
 t)

(provide 'init-one-key)
