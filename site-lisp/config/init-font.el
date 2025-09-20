;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-font.el --- Font configuration

;;; Require
(require 'init-const)
(require 'lazy-load)

;;; Code:

;; (setq-default line-spacing 0.2)

;; https://github.com/Eason0210/emacs.d/blob/master/lisp/init-font.el
(defvar font-list
  (cond
   (sys/mac-p
    '(("JetBrains Mono" . 150) ("SF Mono" . 150) ("Monaco" . 150)))
   (sys/windows-p
    '(("JetBrains Mono" . 100) ("WenQuanYi Micro Hei Mono" . 101) ("Courier New" . 150)))
   (t
    '(("Source Code Pro" . 100) ("Consolas" . 100) ("Cascadia Mono" . 100))))
  "List of fonts and sizes.  The first one available will be used.")

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun change-font ()
  "Set English font from the `font-list'."
  (interactive)
  (let* (available-fonts font-name font-size)
    (dolist (font font-list
             (setq available-fonts (nreverse available-fonts)))
      (when (font-installed-p (car font))
        (push font available-fonts)))
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string
                          (completing-read "What font to use? "
                                           available-fonts nil t)
                          available-fonts)))
            (setq font-name (car chosen)
                  font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts)
              font-size (cdar available-fonts)))
      (set-face-attribute 'default nil :font font-name :height font-size))))

(when (display-graphic-p)
  (change-font)
  (catch 'end
    (dolist (font '("Segoe UI Symbol" "Apple Color Emoji" "Symbola" "Symbol"))
            (when (font-installed-p font)
              (set-fontset-font t 'unicode font nil 'prepend)
              (throw 'end t))))
  (catch 'end                      ;给相应的字符集设置中文字体，这里的字体是。
    (dolist (font '("Microsoft Yahei"))
            (when (font-installed-p font))
            (dolist (charset '(kana han cjk-misc bopomofo chinese-gbk))
              (set-fontset-font t charset (font-spec :name font :size 12)))
            (throw 'end t)))
  )

;;; ### Font ###
;;; --- 字体命令
(lazy-load-set-keys
 '(
   ("s--" . text-scale-decrease)        ;减小字体大小
   ("s-=" . text-scale-increase)        ;增大字体大小
   ))

(provide 'init-font)

;;; init-font.el ends here
