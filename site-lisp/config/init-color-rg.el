
(require 'color-rg)
(require 'lazy-load)

(setq color-rg-show-function-name-p nil)

(defun color-rg-open-file-and-stay-then-quit ()
  "Open current file and stay there then quit color-rg."
  (interactive)
  (let* ((match-file (color-rg-get-match-file))
         (match-line (color-rg-get-match-line))
         (match-column (color-rg-get-match-column)))
    (color-rg-quit)
    (save-excursion
      (let ((inhibit-message t))
        ;; open file
        (find-file match-file)
        ;; Jump to match point.
        (color-rg-move-to-point match-line match-column)))
    ))

(lazy-load-local-keys
 '(("m" . color-rg-open-file-and-stay-then-quit))
 color-rg-mode-map
 "init-color-rg")

(provide 'init-color-rg)