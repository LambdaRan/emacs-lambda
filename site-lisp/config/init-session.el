;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-session.el --- Session save/restore

(require 'auto-save)

;;; Code:

(setq desktop-load-locked-desktop t) ; don't popup dialog ask user, load anyway
(setq desktop-restore-frames nil)    ; don't restore any frame

(defun es-kill-unused-buffers ()
  (interactive)
  (ignore-errors
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (string-prefix-p "*" (buffer-name))
                   (string-suffix-p "*" (buffer-name)))
          (kill-buffer buf))))))

(defun emacs-session-restore ()
  "Restore emacs session."
  (interactive)
  (ignore-errors
    ;; Kill other windows.
    (delete-other-windows)
    ;; Kill unused buffers.
    (es-kill-unused-buffers)
    ;; Restore session.
    (desktop-read (expand-file-name user-emacs-directory))
    ))

(defun emacs-session-save (&optional arg)
  "Save emacs session."
  (interactive "p")
  (ignore-errors
    (if (equal arg 4)
        ;; Kill all buffers if with prefix argument.
        (mapc #'kill-buffer (buffer-list))
      ;; Kill unused buffers.
      (es-kill-unused-buffers)
      ;; Save all buffers before exit.
      (auto-save-buffers))
    ;; Save session.
    (make-directory (expand-file-name user-emacs-directory) t)
    (desktop-save (expand-file-name user-emacs-directory))
    (kill-emacs)))

(provide 'init-session)

;;; init-session.el ends here
