;; -*- coding: utf-8; lexical-binding: t; -*-
(require 'init-const)
(require 'fastctags)

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Don't warn when TAGS files are large
(setq large-file-warning-threshold nil)
(setq fastctags-stop-auto-update-tags t)
;; Use ripgrep instead of git grep for fallback searching
(setq fastctags-use-git-grep-p nil)
(setq fastctags-use-ripgrep-force t)
(setq fastctags-grep-program "rg")

;; 回退 grep 用 rg：忽略项目及父目录的 VCS ignore 规则，保留 .ignore/.rgignore。
;; 用独立参数变量赋值，重新加载配置时也不会重复追加选项。
(setq fastctags-grep-extra-arguments "--no-ignore-vcs")

;; 将该变量标记为始终安全（接受任何值）
(put 'fastctags-extra-tags-files 'safe-local-variable #'always)

;; Completion at point: fastctags + cape-dabbrev 合并
;; 使用 cape-capf-super 将 fastctags 和 dabbrev 的结果合并（两者补全起点一致，可合并）。
;; cape-line 不放入 super：其补全起点为行首(pos-bol)，与词/符号类不同，
;; cape-capf-super 无法合并不同起点的 capf（会被静默丢弃），故改为按需按键触发（见 init-corfu）。
;; 此 hook 必须在 fastctags 加载后注册，确保 fastctags-completion-at-point 可用。
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list (cape-capf-super #'cape-dabbrev
                                               #'fastctags-completion-at-point)
                              #'cape-file
                              #'cape-keyword))))

;; consult-imenu 供 ran-fastctags-imenu 使用（consult 已由 init-vertico 加载）。
;; fastctags 已在文件顶部 require，无需 with-eval-after-load 包裹；semantic 从未启用，
;; 不再加载 semantic/fw、也不保留 (semantic-active-p) 死分支。
(require 'consult-imenu)

(defun ran-fastctags-imenu ()
  "List all imenu tag with consult-imenu or imenu.
consult-imenu 不传 :default，无法自动预选当前符号；
这里从 consult-imenu 缓存中找到包含当前符号的完整候选字符串，
注入 completing-read 的 DEF 参数，vertico 精确匹配后预选。"
  (interactive)
  (if (null (ignore-errors (imenu--make-index-alist)))
      (call-interactively 'imenu)
    (let ((sym (thing-at-point 'symbol))
          (orig-cr (symbol-function #'completing-read)))
      (cl-letf (((symbol-function #'completing-read)
                 (lambda (prompt collection &optional predicate require-match
                                 initial-input hist _def inherit-input-method)
                   ;; 从 consult-imenu 缓存中找包含当前符号的完整候选字符串
                   (let ((def (when sym
                                (seq-find (lambda (item)
                                            (string-match-p
                                             (concat "\\b" (regexp-quote sym) "\\'")
                                             (car item)))
                                          (consult-imenu--items)))))
                     (funcall orig-cr prompt collection predicate require-match
                              initial-input hist (and def (car def))
                              inherit-input-method)))))
        (call-interactively #'consult-imenu)))))

(defun fastctags-nav-find-tag-at-point-in-specific-directory ()
  "Find tag using tagname at point, selecting from specific tags files.
Use `pop-tag-mark' to jump back."
  (interactive)
  (let* ((tagname (fastctags-tagname-at-point)))
    (cond
     (tagname
      (fastctags-nav-find-tag-in-specific-directory tagname buffer-file-name))
     (t
      (message "No tag at point")))))

(defun fastctags-nav-find-tag-in-specific-directory (tagname current-file)
  "Select a tags file from `fastctags-extra-tags-files' and search TAGNAME."
  (when (and fastctags-extra-tags-files tagname)
    (let* ((tagfiles (ff-list-replace-env-vars fastctags-extra-tags-files))
           (curtagfile (fastctags-locate-tags-file))
           (all-files (if curtagfile (cons curtagfile tagfiles) tagfiles))
           (selected (completing-read "Select tag file then search: "
                                      all-files nil t)))
      (when selected
        (let ((default-directory (file-name-directory (expand-file-name selected))))
          (fastctags-nav-find-tag-api tagname nil current-file))))))

;;; ---------------------------------------------------------------------------
;;; 标签缓存补丁（fastctags.el 由 assistant.py 从上游同步，改动只放这里）
;;; 上游按大小和时间间隔重载，大 tags 变小、同大小替换或失效后可能一直读旧缓存。
;;; 补全与导航共用加载 advice：外部变更先观察稳定性，成功读取后才提交新快照。
;;; ---------------------------------------------------------------------------

(defvar ran-fastctags--nav-opened nil
  "本次导航是否真的打开了某个位置，用于判断是否空手而归。")

(defcustom ran-fastctags-tags-settle-delay 1.0
  "外部 tags 的 mtime 和大小在两次查询间保持不变至少多少秒才重载。
等待期间保留旧缓存；手动重载和重建完成 hook 不等待。
这是静默窗口启发式，外部生成器最好使用临时文件加原子替换。"
  :type 'number
  :group 'fastctags)

(defvar ran-fastctags--pending-stamps (make-hash-table :test #'equal)
  "等待稳定的 tags 文件，值为 (STAMP . FIRST-SEEN-TIME)。")

(defvar ran-fastctags--preserve-cache nil
  "强制重建启动期间非 nil，避免后台任务完成前清掉旧快照。")

(defun ran-fastctags--file-stamp (tags-file)
  "返回 TAGS-FILE 的 (MTIME SIZE)，不存在时返回 nil。"
  (when-let* ((attrs (file-attributes tags-file)))
    (list (file-attribute-modification-time attrs) (file-attribute-size attrs))))

(defun ran-fastctags--stamp-settled-p (tags-file stamp)
  "观察 TAGS-FILE 的 STAMP，静默窗口已满足时返回非 nil。"
  (let ((pending (gethash tags-file ran-fastctags--pending-stamps))
        (now (float-time (current-time))))
    (if (and pending (equal stamp (car pending)))
        (>= (- now (cdr pending)) ran-fastctags-tags-settle-delay)
      (puthash tags-file (cons stamp now) ran-fastctags--pending-stamps)
      nil)))

(defun ran-fastctags-cache-invalidate-around (fn tags-file)
  "正常失效 TAGS-FILE；重建启动阶段则保留旧快照直到完成 hook。"
  (unless ran-fastctags--preserve-cache
    (prog1 (funcall fn tags-file)
      (setq fastctags-complete-cached-candidates nil)
      (remhash tags-file ran-fastctags--pending-stamps))))

(defun ran-fastctags-load-tags-around (fn tags-file static-p &optional force _no-diff-prog)
  "稳定读取 TAGS-FILE 后才更新缓存，补全和导航使用同一条路径。
FORCE 或首次加载不等待；外部变更稳定后完整重读，不走仅合并新增符号的 diff。
读取期间文件再次变化或读取失败时保留原缓存。成功提交快照返回 t。"
  (let* ((info (fastctags-get-tags-file-info tags-file))
         (stamp (ran-fastctags--file-stamp tags-file))
         (cached-stamp (and info (list (plist-get info :mtime)
                                      (plist-get info :filesize)))))
    (unless stamp
      (signal 'file-missing (list "Cannot stat tags file" tags-file)))
    (cond
     ((and (not force) (plist-get info :raw-content) (equal stamp cached-stamp))
      (remhash tags-file ran-fastctags--pending-stamps)
      nil)
     ((or force
          (and (not info) (not (gethash tags-file ran-fastctags--pending-stamps)))
          (ran-fastctags--stamp-settled-p tags-file stamp))
      (let ((old-candidates fastctags-complete-cached-candidates)
            committed)
        (unwind-protect
            (progn
              ;; FORCE + NO-DIFF-PROG：不预先失效旧缓存，也不把已删除的符号留在字典中。
              (funcall fn tags-file static-p t t)
              (let ((after-stamp (ran-fastctags--file-stamp tags-file)))
                (if (equal stamp after-stamp)
                    (progn
                      (puthash tags-file
                               (plist-put (fastctags-get-tags-file-info tags-file)
                                          :mtime (car stamp))
                               fastctags-tags-file-cache)
                      ;; extra/static tags 重载也必须清掉合并后的补全候选缓存。
                      (setq fastctags-complete-cached-candidates nil
                            committed t)
                      (remhash tags-file ran-fastctags--pending-stamps)
                      t)
                  (puthash tags-file (cons after-stamp (float-time (current-time)))
                           ran-fastctags--pending-stamps)
                  nil)))
          (unless committed
            (when fastctags-tags-file-cache
              (if info (puthash tags-file info fastctags-tags-file-cache)
                (remhash tags-file fastctags-tags-file-cache)))
            (setq fastctags-complete-cached-candidates old-candidates))))))))

(defun ran-fastctags--check-stamp (tags-file &rest _ignore)
  "导航也检查同大小替换和 extra tags，加载判断统一交给加载 advice。"
  (when (and tags-file (file-exists-p tags-file))
    (fastctags-load-tags-file tags-file nil)))

(defun ran-fastctags-mark-nav-open (&rest _ignore)
  "标记本次导航确实打开了位置。"
  (setq ran-fastctags--nav-opened t))

(defun ran-fastctags-nav-report (fn tagname fuzzy current-file)
  "标签和 grep 回退都无结果时报提示，不再静默失败。"
  (setq ran-fastctags--nav-opened nil)
  (prog1 (funcall fn tagname fuzzy current-file)
    (when (and tagname
               (not ran-fastctags--nav-opened)
               (memq this-command '(fastctags-nav-find-tag-at-point
                                     fastctags-nav-find-tag
                                     fastctags-nav-find-tag-at-point-in-specific-directory)))
      (message "fastctags: 标签与 grep 都无结果 %s（tags 可能过期，M-x fastctags-update-tags-force 重建）"
               tagname))))

(defun ran-fastctags-force-update-around (fn &optional forced-tags-file)
  "临时放开自动更新开关，重建完成前不失效旧缓存。"
  (let ((fastctags-stop-auto-update-tags nil)
        (ran-fastctags--preserve-cache t))
    (funcall fn forced-tags-file)))

(defun ran-fastctags--load-raw (tags-file)
  "立即完整重读 TAGS-FILE；失败时保留旧快照。"
  (let ((beg (current-time)))
    (unless (fastctags-load-tags-file tags-file nil t t)
      (user-error "tags 文件读取期间仍在变化，请稍后重试：%s" tags-file))
    (message "fastctags: 已重读 %s（%.1f 秒）"
             tags-file (float-time (time-subtract (current-time) beg)))))

;;;###autoload
(defun ran-fastctags-reload-tags ()
  "不重启 Emacs 强制重读当前项目的 tags 与 `fastctags-extra-tags-files'。"
  (interactive)
  (let ((files (delq nil
                     (append (list (fastctags-locate-tags-file))
                             (mapcar (lambda (f) (and f (file-exists-p f) f))
                                     (ff-list-replace-env-vars fastctags-extra-tags-files))))))
    (unless files
      (user-error "没找到 tags 文件（`fastctags-locate-tags-file' 返回 nil）"))
    (dolist (f files) (ran-fastctags--load-raw f))))

(defun ran-fastctags-after-update (tags-file)
  "ctags 重建完成后重读缓存并报告 TAGS-FILE 大小。"
  (let* ((size (file-attribute-size (file-attributes tags-file))))
    (ran-fastctags--load-raw tags-file)
    (message "fastctags: %s 已更新（%.1f MB）并重载缓存"
             tags-file (/ size 1048576.0))))

;; 清除旧版 advice，使当前 Emacs 直接重载本配置也能切换到新实现。
(advice-remove #'fastctags-tags-file-cache-invalidate #'ran-fastctags-cache-fix-after-invalidate)
(advice-remove #'fastctags-load-tags-file #'ran-fastctags--remember-mtime)
(advice-add #'fastctags-tags-file-cache-invalidate :around #'ran-fastctags-cache-invalidate-around)
(advice-add #'fastctags-load-tags-file :around #'ran-fastctags-load-tags-around)
(advice-add #'fastctags-nav-extract-cands :before #'ran-fastctags--check-stamp)
(advice-add #'fastctags-open-file-api :before #'ran-fastctags-mark-nav-open)
(advice-add #'fastctags-nav-find-tag-api :around #'ran-fastctags-nav-report)
(advice-add #'fastctags-update-tags-force :around #'ran-fastctags-force-update-around)
(add-hook 'fastctags-after-update-tags-hook #'ran-fastctags-after-update)

(provide 'init-fastctags)
