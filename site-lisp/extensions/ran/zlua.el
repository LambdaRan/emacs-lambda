
;; https://github.com/skywind3000/z.lua
;; https://github.com/abo-abo/swiper

(require 'ivy)

;; (split-string "0.25 /Users/randegang/plugins\n0.25 /Users/randegang/leetcode/Contents\n" "\n" t "[0-9. ]+")
;; (shell-command-to-string "lua ~/lambda/software/z.lua \\-l")
;; https://emacs-china.org/t/shell-command-to-string-bash-builtin-builtin/7421
;; (shell-command-to-string "bash -ic z lambda")

(defgroup zlua nil
  "Use z.lua script jump to some directory"
  :group 'zlua)

(defcustom zlua-path "z.lua"
  "z.lua script path"
  :type 'string
  :group 'zlua)

(defcustom zlua-args ""
  "z.lua script arguments"
  :type 'string
  :group 'zlua)

(defcustom zlua-sort-directory-candidates t
  "Enable sort directory candidates"
  :type 'boolean
  :group 'zlua)

(defvar zlua-debug nil
  "Enable debug mode.")

(defvar cache-zlua-command-can-executable-p nil
  "cache can executable")

(defun zlua-read-input ()
  "Read directly from minibuffer."
  (let* ((current-symbol (ivy-thing-at-point))
         (input-string
          (string-trim
           (read-string
            (format "ZLUA Jump Directory (%s): " current-symbol)
            nil
            'color-rg-read-input-history
            ))))
    (when (string-blank-p input-string)
      (setq input-string current-symbol))
    (if (string= input-string "")
        (setq input-string "/Users/randegang"))
    (when zlua-debug
      (message "User input : %s" input-string))
    input-string))

(defun zlua-build-command (keywords)
  "Create command for z.lua with KEYWORDS"
  (unless cache-zlua-command-can-executable-p
    (unless (executable-find "lua")
      (error "Not find lua, please install lua first."))

    (cond
      ((eq system-type 'windows-nt)
       (if (executable-find "clink")
           (setq cache-zlua-command-can-executable-p t)
         (error "clink is not install.")))
      (t (if (file-exists-p zlua-path)
             (setq cache-zlua-command-can-executable-p t)
           (error "z.lua is not in path")))
      ))
  (let ((command-line))
    (if (eq system-type 'windows-nt)
        (setq command-line (format "z -l %s" keywords))
      (setq command-line (format "%s %s -l %s" (executable-find "lua") zlua-path keywords)))
    (when (memq system-type '(cygwin windows-nt ms-dos))
      (setq command-line (encode-coding-string command-line locale-coding-system)))
    (when zlua-debug
      (message "zlua command : %s" command-line))
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
    (if directory-candidates
        (progn
          (when zlua-sort-directory-candidates
            (setq directory-candidates (reverse directory-candidates)))
          (ivy-read "Zlua jump directory: "
                    directory-candidates
                    :matcher #'counsel--find-file-matcher
                    :initial-input initial-directory
                    :action (lambda (d) (dired-jump nil (expand-file-name d)))
                    :history 'file-name-history
                    :keymap counsel-find-file-map
                    :caller 'zlua-jump-to-directory))
      (message "zlua directory candidates empty"))))

(ivy-set-actions
 'zlua-jump-to-directory
 '(("e" counsel-find-file-extern "open externally")
   ))

(provide 'zlua)

;;; zlua.el ends here
