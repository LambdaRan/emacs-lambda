;; -*- coding: utf-8; lexical-binding: t; -*-

(with-eval-after-load 'ivy
  (require 'counsel)
  (ivy-mode t))

(with-eval-after-load 'ivy
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  ;; Show #/total when scrolling buffers
  (setq ivy-count-format "%d/%d ")
  ;; work around ivy issue.
  ;; @see https://github.com/abo-abo/swiper/issues/828
  (setq ivy-display-style 'fancy)
  ;; https://github.com/redguardtoo/emacs.d/commit/41b8d9feefda14776b24e53bc0454faf7a1794e1
  (setq ivy-dynamic-exhibit-delay-ms 250)
  (defvar my-ivy--queue-last-input nil)
  (defun my-ivy-queue-exhibit@around(f &rest args)
    (cond
     ((equal my-ivy--queue-last-input (ivy--input))
      (ivy--exhibit))
     (t
      (apply f args)))
    (setq my-ivy--queue-last-input (ivy--input)))
  (advice-add #'ivy--queue-exhibit :around #'my-ivy-queue-exhibit@around)
)

(with-eval-after-load 'counsel
  ;; (setq counsel-find-file-at-point t)
  ;; automatically pick up cygwin cli tools for counsel
  (cond
    ((executable-find "rg")
     ;; ripgrep says that "-n" is enabled actually not,
     ;; so we manually add it
     (setq counsel-grep-base-command
           (concat (executable-find "rg")
                   " -n -M 512 --no-heading --color never -i \"%s\" %s")))
    (t (message "ripgrep (rg) not found, counsel-grep will be slow")))
)

;; Integration with `magit'
(with-eval-after-load 'magit
  (setq magit-completing-read-function #'ivy-completing-read))

(provide 'init-ivy)
