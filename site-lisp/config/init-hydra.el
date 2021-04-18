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
 '(("C-c C-d" . #'(lambda ()
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
  ("a" #'(lambda ()
           (interactive)
           (ace-window 1)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)))
  ("v" #'(lambda ()
           (interactive)
           (split-window-right)
           (windmove-right)))
  ("x" #'(lambda ()
           (interactive)
           (split-window-below)
           (windmove-down)))
  ("s" #'(lambda ()
           (interactive)
           (ace-window 4)
           (add-hook 'ace-window-end-once-hook
                     'hydra-window/body)))
  ("S" save-buffer)
  ("d" delete-window)
  ("D" #'(lambda ()
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



(defhydra hydra-source-insight (:hint nil)
  "
Windows^^                             ^Switch^           ^Resize^
------------------------------------------------------------------------------
_c_ Ace           _v_  vertical      _b_ Buffer          _q_ X left
_d_ Del windo     _h_  horizontal    _f_ Find files      _w_ X Down
_D_ Del Other     _C-j_ ace jump     _j_ Next line       _e_ X Top
_SPC_ cancel                         _k_ Previous line   _r_ X Right
"

  ("C-j" awesome-tab-ace-jump)

  ("q" hydra-move-splitter-left)
  ("w" hydra-move-splitter-down)
  ("e" hydra-move-splitter-up)
  ("r" hydra-move-splitter-right)

  ("b" ivy-switch-buffer)
  ("f" counsel-find-file)
  ("j" next-line)
  ("k" previous-line)

  ("d" delete-window)
  ("D" delete-other-windows)

  ("c" #'(lambda ()
           (interactive)
           (ace-window 1)
           (add-hook 'ace-window-end-once-hook
                     'hydra-source-insight/body)))
  ("v" #'(lambda ()
           (interactive)
           (split-window-right)
           (windmove-right)))
  ("h" #'(lambda ()
           (interactive)
           (split-window-below)
           (windmove-down)))

  ("SPC" nil))
(lazy-load-set-keys
 '(("C-c C-n" . hydra-source-insight/body)))

(provide 'init-hydra)
