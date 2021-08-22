;; -*- coding: utf-8; lexical-binding: t; -*-

;; https://github.com/canatella/xwwp
;; https://github.com/philc/vimium

(require 'xwidget)

(defun ran-xwidget-webkit-browse-url-at-point(&optional new-session)
  "Ask a WWW browser to load the URL at or before point with Xwidget-webkit."
  (let ((url (browse-url-url-at-point)))
    (xwidget-webkit-browse-url url new-session)))

(defun ran-xwidget-webkit-browse-url-at-point-new-session ()
  (interactive)
  (ran-xwidget-webkit-browse-url-at-point t))

(defun ran-xwidget-webkit-browse-url-at-point-old-session ()
  (interactive)
  (ran-xwidget-webkit-browse-url-at-point nil))

(with-eval-after-load 'xwidget
  (lazy-load-set-keys
   '(
     ("M-w" . xwidget-webkit-copy-selection-as-kill)
     )
   xwidget-webkit-mode-map))

(provide 'init-xwidget)