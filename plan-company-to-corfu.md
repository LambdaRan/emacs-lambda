# 迁移计划：Company-Mode → Corfu + Cape

## Context

当前 Emacs 配置的 minibuffer 补全已全面现代化（Vertico + Consult + Orderless + Marginalia），但 in-buffer 补全仍使用 company-mode。Corfu 是 Vertico 同作者 (minad) 的 in-buffer 补全 UI，基于标准 Emacs Capf API，与现有 Vertico 生态天然契合。迁移后可统一补全体系、获得 Child Frame 弹窗、并让 Orderless 过滤在 in-buffer 弹窗中也生效。

## 迁移范围分析

### 需要改动的文件

| 文件 | 操作 | 说明 |
|---|---|---|
| `packages.json` | 编辑 | 新增 corfu、cape；移除 company-mode |
| `site-lisp/config/init-corfu.el` | **新建** | 替代 init-company-mode.el |
| `site-lisp/config/init.el` | 编辑 | 增量加载列表中 `init-company-mode` → `init-corfu` |
| `site-lisp/config/init-fastctags.el` | 编辑 | 删除 prog-mode-hook 中自行添加 Capf 的逻辑（第 21-23 行），改由 init-corfu.el 统一管理 |
| `site-lisp/config/init-company-mode.el` | **删除** | 被 init-corfu.el 取代 |

### 无需改动的文件

| 文件 | 原因 |
|---|---|
| `auto-save.el` | **已内置 Corfu 检测**（第 145-147 行检查 `corfu--total`） |
| `init-agent-shell.el` | 不引用 company，agent-shell 的 `:company-kind` 属性 Corfu 也支持 |
| `init-vertico.el` | 已有 Orderless 配置，Corfu 自动继承 `completion-styles` |
| `init-key.el` | 无 company 引用 |

## 实施步骤

### Step 1: 更新 packages.json

- **新增** corfu、cape：
  ```json
  { "name": "corfu", "repo": "minad/corfu", "ref": "main" },
  { "name": "cape",  "repo": "minad/cape",  "ref": "main" }
  ```
- **移除** company-mode 条目（彻底脱离 company 体系）

### Step 2: 同步下载新包

```bash
python assistant.py sync corfu cape
```

### Step 3: 创建 `site-lisp/config/init-corfu.el`

核心配置要点：

**Corfu 基本设置**（对标原 company 配置）：
- `corfu-auto t` + `corfu-auto-delay 0.2`（对标 `company-idle-delay 0.2`）
- `corfu-auto-prefix 2`（对标 `company-minimum-prefix-length 2`）
- `corfu-cycle t`（对标 `company-selection-wrap-around`）
- `corfu-preselect 'prompt`（不预选候选，对标 company 默认行为）
- `corfu-count 14`

**启用扩展**：
- `corfu-history-mode` — 历史排序（替代 company 默认排序）
- `corfu-popupinfo-mode` — 文档预览弹窗
- `corfu-indexed-mode` — 数字索引选择（替代 `company-show-quick-access`）

**按键映射**（Corfu 原生 TAB 语义）：
- `TAB` → `corfu-complete`（Corfu 默认，补全公共前缀）
- `RET` → `corfu-insert`（插入选中项）
- `C-n` / `C-p` → `corfu-next` / `corfu-previous`（对标原配置）
- `M-w` → `corfu-info-location`（对标 `company-show-location`）
- `M-i` → `yas-expand`（保持 yasnippet 快捷方式）
- `M-SPC` → `corfu-insert-separator`（Orderless 多词匹配）

**Cape 补全源**（替代 company backends）：
- 全局默认：`cape-dabbrev` + `cape-file` + `cape-keyword`
- `prog-mode`：通过 `prog-mode-hook` 设置合并 Capf
  - `(cape-capf-super #'fastctags-completion-at-point #'cape-dabbrev)` + `cape-file` + `cape-keyword`
  - `cape-capf-super` 合并多源，对标原 `(company-capf company-dabbrev)` 分组
  - 若 fastctags 尚未加载（增量加载顺序问题），降级为 `cape-dabbrev`

**排除模式**（对标 `company-global-modes`）：
- `global-corfu-mode` **没有**类似 `company-global-modes` 的排除列表，需手动在 mode-hook 中禁用
- 在以下模式中 `(corfu-mode -1)`：
  `shell-mode`, `eshell-mode`, `comint-mode`, `erc-mode`, `gud-mode`, `rcirc-mode`, `text-mode`, `minibuffer-inactive-mode`

### Step 4: 修改 `site-lisp/config/init.el`

第 132 行：`init-company-mode` → `init-corfu`

### Step 5: 修改 `site-lisp/config/init-fastctags.el`

删除第 21-23 行的 `add-hook`（fastctags 自行注册 Capf 到 `completion-at-point-functions`）：

```elisp
;; 删除以下代码：
(add-hook 'prog-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions #'fastctags-completion-at-point nil t)))
```

**原因**：标准 Capf 是"第一个返回结果的就赢"语义。如果 fastctags 和 cape-dabbrev 分别独立注册到 `completion-at-point-functions`，只会有一方的结果出现。必须由 init-corfu.el 统一用 `cape-capf-super` 合并，才能对标原 company 的 `(company-capf company-dabbrev)` 分组行为。

### Step 6: 删除 `site-lisp/config/init-company-mode.el`

### Step 7: 同步并验证

```bash
python assistant.py sync corfu cape
```

重启 Emacs，验证：
1. 编程文件中 typing 2 字符后弹出 Corfu 补全弹窗
2. fastctags 符号补全正常
3. `C-n`/`C-p` 选择、`RET` 插入、`TAB` 补全前缀
4. `M-i` 展开 yasnippet
5. Orderless 无序过滤在弹窗中可用（输入 `buf get` 匹配 `get-buffer`）
6. auto-save 在补全弹窗打开时不保存
7. shell/comint 模式不弹出 Corfu

## 注意事项

1. **company-mode 目录清理**：从 packages.json 移除后，`site-lisp/extensions/company-mode/` 目录暂时保留（不影响运行），后续可手动删除
2. **Windows 兼容性**：Corfu 的 child frame 在 Windows GUI Emacs 上工作正常
3. **Emacs 版本**：如使用 Emacs < 31 的终端模式，需要额外安装 `corfu-terminal`

## Grill 审查记录

审查过程中发现并修正的问题：

1. **🔴 模式排除**：`global-corfu-mode` 没有 `company-global-modes` 那样的排除列表，原 plan 缺少具体实现 → 已补充 `corfu-mode -1` + mode-hook 方案
2. **🔴 fastctags Capf 冲突**：`init-fastctags.el` 自行注册 Capf 会导致"第一个赢"语义下 dabbrev 被跳过 → 已将 Capf 管理统一到 init-corfu.el，新增 Step 5 修改 init-fastctags.el
3. **🟢 `corfu-preselect 'prompt`**：验证为合法值（Corfu 支持 `prompt`/`first`/`valid`/`directory`）
4. **🟢 `corfu-indexed-mode`**：通过 prefix-arg (`M-1`~`M-9`) 选择候选，功能对标 `company-show-quick-access`
5. **🟢 yasnippet**：当前 `company-backends` 中本就没有 `company-yasnippet`，`M-i` 手动触发无功能损失
6. **🟢 auto-save.el**：已内置 Corfu 检测（`corfu--total`），无需改动
7. **🟢 aweshell**：company backend 在 `(when aweshell-auto-suggestion-p ...)` 块内，用户设为 `nil` 为死代码
