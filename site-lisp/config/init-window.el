;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-window.el --- Window navigation menu

;;; Code:

(one-key-create-menu
 "WINDOW-NAVIGATION"
 '(
   (("j" . "Downward") . windmove-down)
   (("k" . "Upward") . windmove-up)
   (("h" . "Leftward") . windmove-left)
   (("l" . "Rightward") . windmove-right)
   (("s" . "Move Down") . buf-move-down)
   (("d" . "Move Up") . buf-move-up)
   (("a" . "Move Left") . buf-move-left)
   (("f" . "Move Right") . buf-move-right)
   (("u" . "Enlarge Down") . (lambda () (interactive) (windresize-up-inwards '-1)))
   (("i" . "Enlarge Up") . (lambda () (interactive) (windresize-down-inwards '-1)))
   (("y" . "Enlarge Left") . (lambda () (interactive) (windresize-right-inwards '-1)))
   (("o" . "Enlarge Right") . (lambda () (interactive) (windresize-left-inwards '-1)))
   (("m" . "Shrink Down") . (lambda () (interactive) (windresize-up-inwards '1)))
   (("," . "Shrink Up") . (lambda () (interactive) (windresize-down-inwards '1)))
   (("n" . "Shrink Left") . (lambda () (interactive) (windresize-right-inwards '1)))
   (("." . "Shrink Right") . (lambda () (interactive) (windresize-left-inwards '1)))
   (("x" . "Outward Window") . outward-window)
   (("c" . "Inward Window") . inward-window)
   (("7" . "Awesome-Tab Left") . awesome-tab-backward-tab)
   (("8" . "Awesome-Tab Right") . awesome-tab-forward-tab)
   (("9" . "Awesome-Tab Next") . awesome-tab-backward-group)
   (("0" . "Awesome-Tab Previous") . awesome-tab-forward-group)
   ((";" . "Kill Buffer") . kill-this-buffer)
   ((":" . "Kill Other Windows") . delete-other-windows)
   (("'" . "Kill Buffer And Window") . delete-current-buffer-and-window)
   (("e" . "List Registers") . list-registers)
   (("r" . "Remember Register") . frame-configuration-to-register)
   (("t" . "Jump Register") . jump-to-register)
   (("g" . "Split Horizontally") . split-window-horizontally)
   (("v" . "Split Vertically") . split-window-vertically)
   )
 t t)

(provide 'init-window)

;;; init-window.el ends here
