
(require 'lazycat-theme)
;; (require 'lazycat-dark-theme)

;; (load-theme 'lazycat-dark t)
;; (load-theme 'lazycat-light t)
(lazycat-theme-load-dark)

;; (setq mode-line-format nil)
(setq-default mode-line-format (remove 'mode-line-buffer-identification mode-line-format))

(let ((bg-mode (frame-parameter nil 'background-mode)))
  (if (eq bg-mode 'dark)
      (custom-set-faces
       '(hl-line ((t (:background "gray22"))))
       '(font-lock-keyword-face ((((class color) (min-colors 88) (background dark)) (:foreground "#EE8822")))))
    (custom-set-faces
     '(hl-line ((t (:background "gray84")))))))

;; 切换主题后更改行高亮颜色
(defun ran-custom-theme ()
  "Toggle lazycat-theme after custom some face. "
  ;; (interactive)
  (let ((bg-mode (frame-parameter nil 'background-mode)))
    (if (eq bg-mode 'dark)
        (custom-set-faces
         '(hl-line ((t (:background "gray22")))))
      (custom-set-faces
       '(hl-line ((t (:background "gray84")))))))
  (set-cursor-color "Red"))
(advice-add 'lazycat-theme-toggle :after 'ran-custom-theme)

(provide 'init-theme)
