# Ivy/Counsel/Swiper → Vertico/Consult/Orderless 迁移计划

## Context

当前配置使用 Ivy/Counsel/Swiper 作为补全框架。目标迁移到更现代、模块化的 Vertico + Consult + Orderless 栈，同时删除不再需要的 counsel-etags、amx、magit。

---

## 第一步：packages.json 更新

### 删除
- `swiper`（abo-abo/swiper）— 包含 ivy/counsel/swiper
- `counsel-etags`（redguardtoo/counsel-etags）— 已不使用

### 新增
```json
{ "name": "vertico", "repo": "minad/vertico", "ref": "main" },
{ "name": "consult", "repo": "minad/consult", "ref": "main" },
{ "name": "orderless", "repo": "oantolin/orderless", "ref": "master" },
{ "name": "marginalia", "repo": "minad/marginalia", "ref": "main" }
```

> amx 保留在 packages.json 中（它是 ran/ 下的手动维护代码），只是不在任何配置中 require。

执行：`python assistant.py sync vertico consult orderless marginalia`

---

## 第二步：创建 init-vertico.el（替换 init-ivy.el）

删除 `site-lisp/config/init-ivy.el`，创建 `site-lisp/config/init-vertico.el`：

```elisp
;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Vertico — 垂直候选 UI
(require 'vertico)
(vertico-mode 1)
(setq vertico-count 15)
(setq vertico-resize nil)
(setq vertico-cycle t)

;;; Orderless — 空格分隔无序匹配
(require 'orderless)
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles partial-completion))
                                      (buffer (styles partial-completion))))

;;; Savehist — 历史持久化
(require 'savehist)
(savehist-mode 1)
(setq history-length 500)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)

;;; Marginalia — 候选注解
(require 'marginalia)
(marginalia-mode 1)

;;; Consult 配置
(require 'consult)
(setq consult-ripgrep-args
      "rg --null --line-buffered --color=never --max-columns=512 --no-heading --line-number -i")

(provide 'init-vertico)
```

---

## 第三步：init.el 更新

将第 114 行：
```elisp
(require 'init-ivy)
```
改为：
```elisp
(require 'init-vertico)
```

---

## 第四步：init-key.el 更新

### 4a. 删除 magit 按键绑定（第 316-319 行）
```elisp
;; 删除这段：
;;; ### Magit ###
(lazy-load-global-keys
 '(("C-c m" . magit-status+))
 "init-git")
```

### 4b. 替换 Ivy/Counsel/Swiper 按键（第 371-397 行）

替换前：
```elisp
;;; --- Counsel/Ivy 补全 ---
(lazy-load-unset-keys '("C-x C-f" "M-x" "C-x b" "M-y"))
...
(lazy-load-global-keys
 '(("C-x C-f" . counsel-find-file)
   ("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   ("C-c M-l" . counsel-locate)
   ("C-c M-d" . counsel-dired-jump)
   ("C-c M-e" . counsel-find-file-extern)
   ("C-c M-f" . counsel-recentf)
   ("C-x b" . ivy-switch-buffer)
   ("C-c c s" . swiper-isearch))
 "init-ivy")
```

替换后：
```elisp
;;; --- Vertico/Consult 补全 ---
(lazy-load-unset-keys '("C-x C-f" "M-x" "C-x b" "M-y" "C-c M-d" "C-c M-f"))

(lazy-load-global-keys
 '(("C-x C-f" . find-file)
   ("M-x" . execute-extended-command)
   ("M-y" . consult-yank-pop)
   ("C-c M-d" . dired-jump)
   ("C-c M-f" . consult-recent-file)
   ("C-x b" . switch-to-buffer)
   ("C-c c s" . consult-line))
 "init-vertico")
```

> 注：`find-file`、`execute-extended-command`、`switch-to-buffer`、`dired-jump` 是 Emacs 内置命令，vertico 自动接管其 UI。
> `counsel-find-file-extern`（C-c M-e）和 `counsel-locate`（C-c M-l）按键直接删除，不再绑定。

### 4c. 删除注释掉的 counsel-etags 按键（第 339-344 行）
```elisp
;; 删除这段注释代码：
;; (lazy-load-global-keys
;;  '(("C-}" . counsel-etags-find-tag)
;;    ("C-]" . counsel-etags-find-tag-at-point)
;;    ("C-c M-i" . ran-counsel-imenu))
;;  "init-etags")
```

---

## 第五步：zlua.el 重写

将 `ivy-read` 改为 `completing-read`，移除所有 ivy/counsel API 依赖。

### 关键变更：
```elisp
;; 删除 (require 'ivy)
;; 删除 (ivy-thing-at-point) → (current-word) 或 (thing-at-point 'symbol)
;; 删除 (ivy-read ...) → (completing-read ...)
;; 删除 (ivy-set-actions ...) → 直接删除
;; 删除 :matcher #'counsel--find-file-matcher
;; 删除 :keymap counsel-find-file-map
;; 删除 counsel-find-file-extern 动作
```

重写后的 `zlua-jump-to-directory`：
```elisp
(defun zlua-jump-to-directory (&optional initial-directory)
  (interactive)
  (let ((directory-candidates
         (split-string
          (shell-command-to-string (zlua-build-command (zlua-read-input)))
          "\n" t "[0-9. ]+")))
    (if directory-candidates
        (progn
          (when zlua-sort-directory-candidates
            (setq directory-candidates (reverse directory-candidates)))
          (let ((selected (completing-read "Zlua jump directory: "
                                           directory-candidates
                                           nil nil initial-directory
                                           'file-name-history)))
            (when selected
              (dired-jump nil (expand-file-name selected)))))
      (message "zlua directory candidates empty"))))
```

---

## 第六步：ran-toolkit.el 更新

### 6a. `ran-counsel-insert-file-path` 改用 `read-file-name`：

```elisp
(defun ran-counsel-insert-file-path ()
  "Insert file path via completing-read."
  (interactive)
  (let ((file (read-file-name "Find file: ")))
    (when file
      (insert (expand-file-name file)))))
```

移除 `(unless (featurep 'counsel) (require 'counsel))`。

### 6b. `ran-git-refs-for` 移除 magit 依赖（第 46-54 行）：

此函数使用 `magit-git-executable`、`magit-read-other-branch-or-commit`、`magit-git-command`，magit 删除后会报错。改写为使用原生 `shell-command`：

```elisp
(defun ran-git-refs-for ()
  "git push origin HEAD:refs/for/[branch]"
  (interactive)
  (let* ((git-exe (or (executable-find "git") (error "git not found")))
         (branch (read-string "Branch (default origin/master): " nil nil "origin/master"))
         (cmd (format "%s push origin HEAD:refs/for/%s" git-exe branch)))
    (when (y-or-n-p (concat "Next run: " cmd))
      (shell-command cmd))))
```

> 或如不再使用 Gerrit workflow，直接删除此函数。

---

## 第七步：init-fastctags.el 更新

```elisp
;; 删除 (require 'counsel)
;; 新增 (require 'consult) — consult-imenu 需要
;; 将 counsel-semantic-or-imenu → consult-imenu
;; 将 counsel--imenu-candidates → 使用内置 imenu--make-index-alist

(with-eval-after-load 'fastctags
  (require 'semantic/fw)
  (require 'consult))          ; 新增：consult-imenu 依赖

(defun ran-fastctags-imenu ()
  "List all imenu tags with consult-imenu or imenu."
  (interactive)
  (if (and (not (semantic-active-p))
           (null (ignore-errors (imenu--make-index-alist))))
      (call-interactively 'imenu)
    (call-interactively 'consult-imenu)))
```

> `init-vertico.el` 已在启动时同步加载（含 consult），增量加载 `init-fastctags` 时 consult 已就绪。此处 `(require 'consult)` 是防御性加载，确保即使单独 require 也不报错。

---

## 第八步：init-ffip.el 更新

```elisp
;; 删除 (require 'ivy) — ffip 内部使用 completing-read，vertico 自动接管
```

`ffip-prefer-ido-mode` 已经是 nil，`ffip-completing-read` 的默认分支使用 `completing-read`，无需其他改动。

---

## 第九步：Magit 清理

### 9a. 删除 `site-lisp/config/init-git.el`

### 9b. init-tempbuf.el — 移除 magit hooks（第 99-101 行）
```elisp
;; 删除这三行：
'magit-process-mode-hook
'magit-diff-mode-hook
'magit-status-mode-hook
```

### 9c. init-diff-hl.el — 移除 magit hook（第 30 行）
```elisp
;; 删除：
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
```

### 9d. init-generic.el — 更新注释（第 28 行）
```elisp
;; 原：(setq async-bytecomp-allowed-packages nil) ;避免magit报错
;; 改：(setq async-bytecomp-allowed-packages nil) ;避免 async 字节编译冲突
;; 保留设置本身（对其他包也有用），只更新注释
```

---

## 第十步：清理 packages.json 中的 amx

amx 在 `site-lisp/extensions/ran/` 下（手动维护），packages.json 中**未列出**，无需从 packages.json 删除。保留在 extensions/ran/ 目录即可，不影响运行。

---

## 第十一步：第三方包中的 magit/ivy 引用（无需处理）

以下第三方包包含 magit 或 ivy 引用，但都是安全的——magit 未安装时 hook 不会触发，face 不会应用：

- `diff-hl/diff-hl.el` — 内部定义了 `diff-hl-magit-post-refresh` 和 magit hooks，由 `with-eval-after-load 'magit` 保护
- `awesome-tab/awesome-tab.el` — `memq major-mode '(magit-xxx-mode)` 检查，magit 不存在时返回 nil
- `all-the-icons/all-the-icons.el` — magit 模式的图标定义，不加载则无影响
- `lazycat-theme/lazycat-theme.el` — `magit-*` face 定义，不加载则不应用
- `aweshell/eshell-prompt-extras.el` — 仅注释提及

这些无需修改，不会引起启动报错。

---

## 涉及文件总览

| 文件 | 操作 |
|------|------|
| `packages.json` | 删除 swiper/counsel-etags，新增 vertico/consult/orderless/marginalia |
| `site-lisp/config/init-ivy.el` | **删除** |
| `site-lisp/config/init-vertico.el` | **新建** |
| `site-lisp/config/init.el` | 改 require |
| `site-lisp/config/init-key.el` | 替换按键、删除 magit/注释代码 |
| `site-lisp/config/init-git.el` | **删除** |
| `site-lisp/extensions/ran/zlua.el` | ivy-read → completing-read |
| `site-lisp/extensions/ran/ran-toolkit.el` | 移除 counsel 依赖 + 重写 `ran-git-refs-for` |
| `site-lisp/config/init-fastctags.el` | counsel → consult |
| `site-lisp/config/init-ffip.el` | 删除 `(require 'ivy)` |
| `site-lisp/config/init-tempbuf.el` | 删除 magit hooks |
| `site-lisp/config/init-diff-hl.el` | 删除 magit hook |
| `site-lisp/config/init-generic.el` | 清理注释（可选） |

## 执行顺序

1. `python assistant.py sync vertico consult orderless marginalia`（下载新包）
2. 创建 `init-vertico.el`
3. 修改 `init.el`
4. 修改 `init-key.el`
5. 修改 `zlua.el`
6. 修改 `ran-toolkit.el`
7. 修改 `init-fastctags.el`
8. 修改 `init-ffip.el`
9. 删除 `init-ivy.el`、`init-git.el`
10. 清理 `init-tempbuf.el`、`init-diff-hl.el`
11. 更新 `packages.json`（删除旧包条目）
12. 删除 `site-lisp/extensions/swiper/`、`site-lisp/extensions/counsel-etags/` 目录
13. 重启 Emacs 验证

## 验证清单

- [ ] `M-x` 正常弹出 vertico 候选列表
- [ ] `C-x C-f` 打开文件正常
- [ ] `C-x b` 切换 buffer 正常
- [ ] `C-c c s` (consult-line) buffer 内搜索正常
- [ ] `M-y` (consult-yank-pop) 粘贴历史正常
- [ ] `C-c M-f` (consult-recent-file) 最近文件正常
- [ ] `C-c }` / `C-c ]` (fastctags) 代码跳转正常
- [ ] `C-c M-i` (consult-imenu) imenu 跳转正常
- [ ] `C-c c z` (zlua) 目录跳转正常
- [ ] ffip (`C-c e/f/s`) 项目文件查找正常
- [ ] magit 相关无报错（已删除）
- [ ] `ran-git-refs-for` 仍可调用（或已删除）
- [ ] `*Messages*` 无 ivy/counsel/swiper 相关警告
