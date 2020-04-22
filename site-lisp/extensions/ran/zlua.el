

;; https://github.com/skywind3000/z.lua
;; https://github.com/abo-abo/swiper

(require 'ivy)


;; (split-string "0.25 /Users/randegang/plugins\n0.25 /Users/randegang/leetcode/Contents\n" "\n" t "[0-9. ]+")
;; (shell-command-to-string "lua ~/lambda/software/z.lua \\-l")
;; https://emacs-china.org/t/shell-command-to-string-bash-builtin-builtin/7421
;; (shell-command-to-string "bash -ic z lambda")

(defvar zlua-path "~/lambda/software/z.lua"
  "z.lua script path")

(defvar cache-zlua-command-can-executable-p nil
  "cache can execute")

(defvar zlua-args ""
  "z.lua arguments")

(defun zlua-read-input ()
  "Read directly from minibuffer."
  (let* ((current-symbol (ivy-thing-at-point))
         (input-string
          (string-trim
           (read-string
            (format "Z_LUA Jump Dir (%s): " current-symbol)
            nil
            'color-rg-read-input-history
            ))))
    (when (string-blank-p input-string)
      (setq input-string current-symbol))
    input-string))

(defun zlua-build-command (keywords)
  "Create command for z.lua with KEYWORDS"
  (unless cache-zlua-command-can-executable-p
    (if (and (executable-find "lua")
             (file-exists-p zlua-path))
        (setq cache-zlua-command-can-executable-p t)
      (error "lua and z.lua is not in path")))
  (let ((command-line))
    (setq command-line (format "%s %s -l %s" (executable-find "lua") zlua-path keywords))
    (when (memq system-type '(cygwin windows-nt ms-dos))
      (setq command-line (encode-coding-string command-line locale-coding-system)))
    command-line))

(defun zlua-jump-to-directory (&optional initial-directory)
  "Get directory with zlua and jump to it.
List all directories within the zlua db stored.
INITIAL-DIRECTORY can be given as the initial minibuffer input."
  (interactive)
  (let ((directory-candidates
          (split-string
           (shell-command-to-string (zlua-build-command (zlua-read-input)))
           "\n" t "[0-9. ]+")))
    ;; (message "directory: %s" directory-candidates)
    (when directory-candidates
      (ivy-read "Zlua jump directory: "
                directory-candidates
                :matcher #'counsel--find-file-matcher
                :initial-input initial-directory
                :action (lambda (d) (dired-jump nil (expand-file-name d)))
                :history 'file-name-history
                :keymap counsel-find-file-map
                :caller 'zlua-jump-to-directory))))

(provide 'zlua)
;;; zlua.el ends here
