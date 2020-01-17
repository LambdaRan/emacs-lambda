
(require 'lazy-load)
(require 'projectile)

;; 默认全局使用
;; (projectile-global-mode)
(projectile-mode +1)

(lazy-load-set-keys
 '(
   ("C-c f" . projectile-find-file)
   ("C-c p" . projectile-command-map) ; 定义命令前缀
   )
 projectile-mode-map)

(setq projectile-mode-line-prefix ""
      projectile-sort-order 'recentf
      projectile-use-git-grep t)

;; 启用缓存
;; (setq projectile-enable-caching t)

;; Use the faster searcher to handle project files: ripgrep `rg'.
(when (and (not (executable-find "fd"))
           (executable-find "rg"))
  (setq projectile-generic-command
        (let ((rg-cmd ""))
          (dolist (dir projectile-globally-ignored-directories)
            (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
          (concat "rg -0 --files --color=never --hidden" rg-cmd))))


(provide 'init-projectile)

