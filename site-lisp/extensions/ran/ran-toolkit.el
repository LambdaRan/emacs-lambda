

(defun ran-get-full-path ()
  "copy buffer file full path to kill ring"
  (interactive)
  (if buffer-file-name
      (progn
        (kill-new buffer-file-name)
        (message "ffname : %s" buffer-file-name))
    (message "empty buffer file name")))

;; https://emacs.stackexchange.com/questions/39105/insert-file-path-via-counsel
(defun ran-counsel-insert-file-path ()
  "Insert file path."
  (interactive)
  (unless (featurep 'counsel) (require 'counsel))
  (ivy-read "Find file: " 'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :action
            (lambda (x)
              (insert x))))

(provide 'ran-toolkit)

;;; basic-toolkit.el ends here
