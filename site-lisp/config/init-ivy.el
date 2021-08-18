;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'ivy)
(require 'counsel)

(when sys/mac-cocoa-p
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(ivy-mode t)

;; (with-eval-after-load 'ivy
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-wrap t)
;; Show #/total when scrolling buffers
(setq ivy-count-format "%d/%d ")
;; work around ivy issue.
;; @see https://github.com/abo-abo/swiper/issues/828
(setq ivy-display-style 'fancy)
;; )

;; (with-eval-after-load 'counsel
;; (setq counsel-find-file-at-point t)
;; automatically pick up cygwin cli tools for counsel
(cond
  ((executable-find "rg")
   ;; ripgrep says that "-n" is enabled actually not,
   ;; so we manually add it
   (setq counsel-grep-base-command
         (concat (executable-find "rg")
                 " -n -M 512 --no-heading --color never -i \"%s\" %s")))
  ('t (message "Not find rg")))
;; )

;; Integration with `magit'
(with-eval-after-load 'magit
  (setq magit-completing-read-function 'ivy-completing-read))

(provide 'init-ivy)
