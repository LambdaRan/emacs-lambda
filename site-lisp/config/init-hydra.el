;; -*- coding: utf-8; lexical-binding: t; -*-

;; @see https://github.com/abo-abo/hydra
;; color could: red, blue, amaranth, pink, teal

(require 'hydra)
(require 'lazy-load)



(defhydra hydra-dired-shortkey (:hint nil)
  "
SortFile                  FilterFile
[_s_] Size                 [_S_] Symlink
[_x_] Extension            [_X_] Extension
[_n_] Name                 [_N_] Name
[_t_] Modified Time        [_e_] Execute
[_u_] Access Time          [_f_] File
[_c_] Create Time          [_._] Dot files
[_x_] xxxx                 [_r_] Regex
[_x_] xxxx                 [_d_] Directory
"
  ("s" dired-sort-size)
  ("x" dired-sort-extension)
  ("n" dired-sort-name)
  ("t" dired-sort-time)
  ("u" dired-sort-utime)
  ("c" dired-sort-ctime)

  ("S" dired-filter-by-symlink)
  ("X" dired-filter-by-extension)
  ("N" dired-filter-by-name)
  ("e" dired-filter-by-executable)
  ("f" dired-filter-by-file)
  ("." dired-filter-by-dot-files)
  ("r" dired-filter-by-regexp)
  ("d" dired-filter-by-directory)

  ;; ("jw" ace-jump-word-mode)
  ;; ("jc" ace-jump-char-mode)
  ;; ("jl" ace-jump-line-mode)

  ("q" nil "quit"))

(lazy-load-set-keys
 '(("C-c C-d" . (lambda ()
                  (interactive)
                  (require 'dired-sort)
                  ;; https://github.com/Fuco1/dired-hacks
                  (require 'dired-filter)
                  (hydra-dired-shortkey/body)))))



(defhydra hydra-awesome-tab (:hint nil)
  "
 ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
   ^_k_^   prev group    | _C-a_^^     select first | _b_ search buffer | _C-k_   kill buffer
 _h_   _l_  switch tab   | _C-e_^^     select last  | _g_ search group  | _C-S-k_ kill others in group
   ^_j_^   next group    | _C-j_^^     ace jump     | ^^                | ^^
 ^^ ^^                   | _C-h_/_C-l_ move current | ^^                | ^^
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
"
  ("h" awesome-tab-forward-group)
  ("l" awesome-tab-backward-group)
  ("j" awesome-tab-forward-tab)
  ("k" awesome-tab-backward-tab)
  ("C-a" awesome-tab-select-beg-tab)
  ("C-e" awesome-tab-select-end-tab)
  ("C-j" awesome-tab-ace-jump)
  ("C-h" awesome-tab-move-current-tab-to-left)
  ("C-l" awesome-tab-move-current-tab-to-right)
  ("g" awesome-tab-counsel-switch-group)
  ("C-k" kill-current-buffer)
  ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)

  ("b" ivy-switch-buffer)
  ("f" dired-jump)

  ("q" nil "quit"))

;; (global-set-key (kbd "C-c C-v") 'hydra-awesome-tab/body)

(lazy-load-set-keys
 '(("C-c C-t" . hydra-awesome-tab/body)))



(eval-after-load 'find-file-in-project
  '(progn
     (defhydra hydra-ffip-diff-group (:color blue)
       "
[_k_] Previous hunk
[_j_] Next hunk
[_p_] Previous file
[_n_] Next file
"
       ("k" diff-hunk-prev)
       ("j" diff-hunk-next)
       ("p" diff-file-prev)
       ("n" diff-file-next)
       ("q" nil))))
(defun ffip-diff-mode-hook-hydra-setup ()
  (local-set-key (kbd "C-c C-y") 'hydra-ffip-diff-group/body))
(add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-hydra-setup)


;; {{ @see https://github.com/abo-abo/hydra/wiki/Window-Management

;; helpers from https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let* ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defhydra hydra-window (:hint nil)
  "
Movement^^   ^Split^         ^Switch^     ^Resize^
-----------------------------------------------------
_h_ Left     _v_ ertical     _b_uffer     _q_ X left
_j_ Down     _x_ horizontal  _f_ind files _w_ X Down
_k_ Top      _z_ undo        _a_ce 1      _e_ X Top
_l_ Right    _Z_ reset       _s_wap       _r_ X Right
_F_ollow     _D_elete Other  _S_ave       max_i_mize
_SPC_ cancel _o_nly this     _d_elete
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)

  ("q" hydra-move-splitter-left)
  ("w" hydra-move-splitter-down)
  ("e" hydra-move-splitter-up)
  ("r" hydra-move-splitter-right)

  ("b" ivy-switch-buffer)
  ("f" counsel-find-file)
  ("F" follow-mode)
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)))
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("S" save-buffer)
  ("d" delete-window)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("o" delete-other-windows)
  ("i" ace-delete-other-windows)
  ("z" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
  ("Z" winner-redo)
  ("SPC" nil))
;; (global-set-key (kbd "C-c C-w") 'hydra-window/body)
(lazy-load-unset-keys '("C-c C-w" ))
(lazy-load-set-keys
 '(("C-c C-f" . hydra-window/body)))
;; }}

;; {{
(defhydra hydra-git (:hint nil)
"
Git:
[_s_] Magit status         [_u_] Magit push to remote          [_v_] diff-hl-diff-goto-hunk
[_c_] Magit checkout       [_p_] Magit delete remote branch    [_n_] diff-hl-revert-hunk
[_C_] Magit commit         [_m_] Magit submodule add           [_j_] diff-hl-next-hunk
[_i_] Magit pull           [_d_] Magit submodule remove        [_k_] diff-hl-previous-hunk
[_r_] Magit rebase         [_M_] Magit submodule list
[_e_] Magit merge          [_D_] Magit discarded
[_l_] Magit log            [_,_] Magit init
[_L_] Magit blame          [_._] Magit add remote
[_b_] Magit branch         [_B_] Magit buffer
"
  ("s" magit-status+)
  ("c" magit-checkout)
  ("C" magit-commit)
  ("u" magit-push-current-to-pushremote)
  ("p" magit-delete-remote-branch)
  ("i" magit-pull-from-upstream)
  ("r" magit-rebase)
  ("e" magit-merge)
  ("l" magit-log-all)
  ("L" magit-blame+)
  ("b" magit-branch)
  ("B" magit-process-buffer)
  ("m" magit-submodule-add+)
  ("d" magit-submodule-remove+)
  ("M" magit-list-submodules)
  ("D" magit-discard)
  ("," magit-init)
  ("." magit-remote-add)

  ("v" diff-hl-diff-goto-hunk)
  ("n" diff-hl-revert-hunk)
  ("j" diff-hl-next-hunk)
  ("k" diff-hl-previous-hunk)

  ("q" nil))
;; (global-set-key (kbd "C-c C-g") 'hydra-git/body)
(lazy-load-set-keys
 '(("C-c C-v" . (lambda ()
                  (interactive)
                  (require 'init-git)
                  (hydra-git/body)))))
;; }}

(defhydra hydra-describe (:color blue :hint nil)
  "
Describe Something: (q to quit)
_a_ all help for everything screen    _b_ bindings
_B_ personal bindings                 _c_ char
_C_ coding system                     _f_ function
_F_ flycheck checker                  _i_ input method
_k_ key briefly                       _K_ key
_l_ language environment              _L_ mode lineage
_m_ major mode                        _M_ minor mode
_n_ current coding system briefly     _N_ current coding system full
_o_ lighter indicator                 _O_ lighter symbol
_p_ package                           _P_ text properties
_s_ symbol                            _t_ theme
_v_ variable                          _w_ where is something defined
"
  ("b" describe-bindings)
  ("B" describe-personal-keybindings)
  ("C" describe-categories)
  ("c" describe-char)
  ("C" describe-coding-system)
  ("f" describe-function)
  ("F" flycheck-describe-checker)
  ("i" describe-input-method)
  ("K" describe-key)
  ("k" describe-key-briefly)
  ("l" describe-language-environment)
  ("L" help/parent-mode-display)
  ("M" describe-minor-mode)
  ("m" describe-mode)
  ("N" describe-current-coding-system)
  ("n" describe-current-coding-system-briefly)
  ("o" describe-minor-mode-from-indicator)
  ("O" describe-minor-mode-from-symbol)
  ("p" describe-package)
  ("P" describe-text-properties)
  ("q" nil)
  ("a" help)
  ("s" describe-symbol)
  ("t" describe-theme)
  ("v" describe-variable)
  ("w" where-is))
;; (global-set-key (kbd "C-c C-q") 'hydra-describe/body)
(lazy-load-set-keys
 '(("C-c M-q" . hydra-describe/body)))

(provide 'init-hydra)
