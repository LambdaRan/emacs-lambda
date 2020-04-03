

(require 'counsel-etags)
(require 'lazy-load)

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)

(when (featurep 'cocoa)
  ;; Mac's default ctags does not support -e option
  ;; If you install Emacs by homebrew, another version of etags is already installed which does not need -e too
  ;; the best option is to install latest ctags from sf.net
  (setq ctags-command "/usr/local/bin/ctags -e -R "))

;; Ignore files above 800kb
(setq counsel-etags-max-file-size 800)
;; Ignore build directories for tagging
;; (add-to-list 'counsel-etags-ignore-directories '".vscode")
;; (push "build" counsel-etags-ignore-directories)

;; How many seconds to wait before rerunning tags for auto-update
;; (setq counsel-etags-update-interval 180)
;; Set up auto-update

(push "TAGS" counsel-etags-ignore-filenames)

(add-hook
 'prog-mode-hook
 (lambda () (add-hook 'after-save-hook
                      (lambda ()
                        (counsel-etags-virtual-update-tags)))))

(lazy-load-set-keys
 '(
   ("C-]" . counsel-etags-find-tag-at-point)))


(provide 'init-etags)