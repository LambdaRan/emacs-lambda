

(require 'counsel-etags)
(require 'lazy-load)

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)

;; Ignore files above 800kb
(setq counsel-etags-max-file-size 800)
;; Ignore build directories for tagging
;; (add-to-list 'counsel-etags-ignore-directories '".vscode")
;; (push "build" counsel-etags-ignore-directories)

;; How many seconds to wait before rerunning tags for auto-update
(setq counsel-etags-update-interval 180)
;; Set up auto-update

(add-hook
 'prog-mode-hook
 (lambda () (add-hook 'after-save-hook
                      (lambda ()
                        (counsel-etags-virtual-update-tags)))))

;; The function provided by counsel-etags is broken (at least on Linux)
;; and doesn't correctly exclude directories, leading to an excessive
;; amount of incorrect tags. The issue seems to be that the trailing '/'
;; in e.g. '*dirname/*' causes 'find' to not correctly exclude all files
;; in that directory, only files in sub-directories of the dir set to be
;; ignore.
(defun my-scan-dir (src-dir &optional force)
  "Create tags file from SRC-DIR. \
     If FORCE is t, the commmand is executed without \
     checking the timer."
  (let* ((find-pg (or
                   counsel-etags-find-program
                   (counsel-etags-guess-program "find")))
         (ctags-pg (or
                    counsel-etags-tags-program
                    (format "%s -e -L" (counsel-etags-guess-program
                                        "ctags"))))
         (default-directory src-dir)
         ;; run find&ctags to create TAGS
         (cmd (format
               "%s . \\( %s \\) -prune -o -type f -not -size +%sk %s | %s -"
               find-pg
               (mapconcat
                (lambda (p)
                  (format "-iwholename \"*%s*\"" p))
                counsel-etags-ignore-directories " -or ")
               counsel-etags-max-file-size
               (mapconcat (lambda (n)
                            (format "-not -name \"%s\"" n))
                          counsel-etags-ignore-filenames " ")
               ctags-pg))
         (tags-file (concat (file-name-as-directory src-dir) "TAGS"))
         (doit (or force (not (file-exists-p tags-file)))))
    ;; always update cli options
    (when doit
      (message "%s at %s" cmd default-directory)
      (async-shell-command cmd)
      (visit-tags-table tags-file t))))

(setq counsel-etags-update-tags-backend
      (lambda ()
        (interactive)
        (let* ((tags-file (counsel-etags-locate-tags-file)))
          (when tags-file
            (my-scan-dir (file-name-directory tags-file) t)
            (run-hook-with-args
             'counsel-etags-after-update-tags-hook tags-file)
            (unless counsel-etags-quiet-when-updating-tags
              (message "%s is updated!" tags-file))))))



(lazy-load-set-keys
 '(
   ("C-]" . counsel-etags-find-tag-at-point)))


(provide 'init-etags)