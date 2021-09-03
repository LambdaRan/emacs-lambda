;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'init-const)
(require 'counsel-etags)

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

;; Ignore files above 800kb
(setq counsel-etags-max-file-size 800)
;; Ignore build directories for tagging
;; (add-to-list 'counsel-etags-ignore-directories '".vscode")
;; (push "build" counsel-etags-ignore-directories)

;; How many seconds to wait before rerunning tags for auto-update
;; (setq counsel-etags-update-interval 180)

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

(unless sys/windows-p
  (add-hook 'prog-mode-hook
            #'(lambda ()
                (add-hook 'after-save-hook
                          'counsel-etags-virtual-update-tags 'append 'local))))

(defun counsel-etags-find-tag-at-point-in-specific-directory ()
  "Find tag using tagname at point.  Use `pop-tag-mark' to jump back.
Please note parsing tags file containing line with 2K characters could be slow.
That's the known issue of Emacs Lisp.  The program itself is perfectly fine."
  (interactive)
  (let* ((tagname (counsel-etags-tagname-at-point)))
    (cond
     (tagname
      (counsel-etags-find-tag-in-specific-directory tagname buffer-file-name))
     (t
      (message "No tag at point")))))

(defun counsel-etags-find-tag-in-specific-directory (tagname current-file)
  (when (and counsel-etags-extra-tags-files tagname)
    (dolist (file (ff-list-replace-env-vars counsel-etags-extra-tags-files))
      (message "load %s in %s" file counsel-etags-extra-tags-files))
    (let ((tagfiles (ff-list-replace-env-vars counsel-etags-extra-tags-files))
          (curtagfile (counsel-etags-locate-tags-file)))
      (setq tagfiles (cons curtagfile tagfiles))
      (ivy-read "Select tag file then search: "
                tagfiles
                :action (lambda (file)
                          (let ((default-directory (file-name-directory (expand-file-name file))))
                            (counsel-etags-find-tag-api tagname nil current-file)))
                :history 'file-name-history
                :keymap counsel-find-file-map
                :caller 'counsel-etags-find-tag-in-specific-directory)
      )))

(provide 'init-etags)