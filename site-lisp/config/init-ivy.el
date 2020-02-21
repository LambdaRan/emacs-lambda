
(require 'ivy)
(require 'counsel)
(require 'lazy-load)

(when (featurep 'cocoa)
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))


(ivy-mode t)

(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-wrap t)
;; Show #/total when scrolling buffers
(setq ivy-count-format "%d/%d ")
;; work around ivy issue.
;; @see https://github.com/abo-abo/swiper/issues/828
(setq ivy-display-style 'fancy)

;; (setq counsel-find-file-at-point t)
(eval-after-load 'counsel
  '(progn
    ;; automatically pick up cygwin cli tools for counsel
    (cond
      ((executable-find "rg")
       ;; ripgrep says that "-n" is enabled actually not,
       ;; so we manually add it
       (setq counsel-grep-base-command
             (concat (executable-find "rg")
                     " -n -M 512 --no-heading --color never -i \"%s\" %s")))
      ('t
       (message "Not find rg")))
    ;; @see https://oremacs.com/2015/07/23/ivy-multiaction/
    ;; press "M-o" to choose ivy action
    ;; (ivy-set-actions
    ;;  'counsel-find-file
    ;;  '(("j" find-file-other-frame "other frame")
    ;;    ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
    ;;    ("x" counsel-find-file-extern "open externally")
    ;;    ("d" delete-file "delete")
    ;;    ("r" counsel-find-file-as-root "open as root")))
    ))


;;; ### Unset key ###
;;; --- 卸载按键
(lazy-load-unset-keys                   ;全局按键的卸载
                      '("C-x C-f" "M-x" "C-x b"))

(lazy-load-set-keys
 '(
   ("C-x C-f" . counsel-find-file)
   ("M-x" . counsel-M-x)
   ("C-x b" . ivy-switch-buffer)
   ))

;; Integration with `projectile'
;; (with-eval-after-load 'projectile
;;   (setq projectile-completion-system 'ivy))

;; Integration with `magit'
(with-eval-after-load 'magit
  (setq magit-completing-read-function 'ivy-completing-read))



(provide 'init-ivy)