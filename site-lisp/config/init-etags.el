
(require 'counsel-etags)
(require 'lazy-load)
(require 'init-const)

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)

(when sys/mac-cocoa-p
  ;; Mac's default ctags does not support -e option
  ;; If you install Emacs by homebrew, another version of etags is already installed which does not need -e too
  ;; the best option is to install latest ctags from sf.net
  (setq ctags-command "/usr/local/bin/ctags -e -R "))

(when sys/windows-p
  (setq counsel-etags-ctags-options-file "~\.ctags")
  )
;; Ignore files above 800kb
(setq counsel-etags-max-file-size 800)
;; Ignore build directories for tagging
;; (add-to-list 'counsel-etags-ignore-directories '".vscode")
;; (push "build" counsel-etags-ignore-directories)

;; How many seconds to wait before rerunning tags for auto-update
;; (setq counsel-etags-update-interval 180)
;; Set up auto-update

;; (setq counsel-etags-debug nil)
;; (setq counsel-etags-grep-program (counsel-etags-guess-program "rg"))

;; (setq imenu-create-index-function 'counsel-etags-imenu-default-create-index-function)
(defun ran-counsel-imenu()
  "List all imenu tag with counsel-semantic-or-imenu or counsel-etags-list-tag-in-current-file"
  (interactive)
  (require 'semantic/fw)
  (if (and (not (semantic-active-p))
           (seq-empty-p (counsel--imenu-candidates)))
      (call-interactively 'counsel-etags-list-tag-in-current-file)
    (call-interactively 'counsel-semantic-or-imenu)))

(add-hook 'prog-mode-hook
          #'(lambda ()
              (add-hook 'after-save-hook
                        'counsel-etags-virtual-update-tags 'append 'local)))

(lazy-load-set-keys
 '(
   ("C-]" . counsel-etags-find-tag-at-point)
   ("C-c M-i" . ran-counsel-imenu)))


(provide 'init-etags)