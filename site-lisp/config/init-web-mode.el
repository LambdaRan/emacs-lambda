(require 'web-mode)

(setq web-mode-enable-auto-quoting nil) ;disable automatic insertion of double quotes, not easy to use if cursor in string

;; Emmit.
(setq web-mode-tag-auto-close-style 2) ;2 mean auto-close with > and </.
(setq web-mode-markup-indent-offset 2)

(provide 'init-web-mode)

;;; init-web-mode.el ends here
