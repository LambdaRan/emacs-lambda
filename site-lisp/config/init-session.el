;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-session.el --- Session save/restore

(require 'auto-save)

(setq desktop-load-locked-desktop t) ; don't popup dialog ask user, load anyway
(setq desktop-restore-frames nil)    ; don't restore any frame

;;; 受保护的 buffer 列表（不会被 es-kill-unused-buffers 杀掉）
(defvar es-protected-buffers
  '("*scratch*" "*Messages*" "*Warnings*" "*Help*" "*Async-native-compile-log*")
  "不会被 `es-kill-unused-buffers' 杀掉的 buffer 名称列表。")

(defun es-kill-unused-buffers ()
  "杀掉所有 *...* 形式的 buffer，但保留 `es-protected-buffers' 中的。"
  (interactive)
  (ignore-errors
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        (when (and (string-prefix-p "*" name)
                   (string-suffix-p "*" name)
                   (not (member name es-protected-buffers)))
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
    ;; 清理 Minibuf-0：awesome-tray 向其中写入了 tray 文本，
    ;; desktop-read 可能将该非空 buffer 恢复到窗口中，导致 tray 显示在顶部。
    (when (get-buffer " *Minibuf-0*")
      (with-current-buffer " *Minibuf-0*"
        (delete-region (point-min) (point-max))))
    ;; 如果 *Minibuf-0* 被显示在某个窗口中，替换为 *scratch*
    (dolist (win (window-list))
      (when (equal (buffer-name (window-buffer win)) " *Minibuf-0*")
        (with-selected-window win
          (switch-to-buffer "*scratch*"))))))

(defun emacs-session-save (&optional no-quit)
  "Save emacs session and quit.
With prefix argument (C-u), save without quitting."
  (interactive "P")
  (ignore-errors
    (es-kill-unused-buffers)
    ;; Save all buffers before exit.
    (auto-save-buffers)
    ;; Save session.
    (make-directory (expand-file-name user-emacs-directory) t)
    (desktop-save (expand-file-name user-emacs-directory)))
  (unless no-quit
    (kill-emacs)))

(provide 'init-session)
