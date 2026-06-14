# CLAUDE.md

本文件为 Claude Code (claude.ai/code) 在此仓库中工作时提供指导。

## 项目概述

个人 Emacs 配置（以 Windows 为主，跨平台）。使用自定义包管理器（`assistant.py`）替代 Emacs 内置的 `package.el`。所有第三方包直接放置在 `site-lisp/extensions/` 下，由 `assistant.py` 从 GitHub Archive 下载 zip 包并解压同步（不使用 git submodule，`.gitmodules` 为历史遗留文件）。

## 包管理命令

```bash
# 同步所有包（从 GitHub 下载压缩包并解压，应用排除规则）
python assistant.py sync --all

# 同步指定包
python assistant.py sync company-mode yasnippet

# 添加新包到 packages.json 并立即同步
python assistant.py add owner/repo --ref main --sync

# 标记为手动维护（不自动同步）
python assistant.py add owner/repo --manual --note "说明"
```

`packages.json` 中标记 `"manual": true` 的包（ghostel、vterm、lazycat、lazycat-theme、ran）不会自动同步——它们包含原生 DLL 或为个人维护的代码。

## 目录结构

```
setup/
  early-init.el      → 复制到 ~/.emacs.d/early-init.el
  site-start.el      → 复制为 ~/.emacs（入口文件，将 site-lisp/ 加入 load-path）
site-lisp/
  start.el           → 由 site-start.el 加载，require init
  config/            → init-*.el 配置模块（每个功能/包一个文件）
  extensions/        → 第三方包（由 assistant.py 从 GitHub 下载）
    lazycat/         → 个人工具集合（手动维护）
    lazycat-theme/   → 个人主题（手动维护）
    ran/             → 个人工具（手动维护）
    ghostel/         → 手动维护，含原生 DLL
    vterm/           → 手动维护，含原生 DLL
packages.json        → 包清单（name、repo、ref、ignores、files）
assistant.py         → 包管理器（从 GitHub Archive 同步）
```

## 加载顺序

1. `setup/site-start.el`（作为 `~/.emacs`）— 将 `site-lisp/` 子目录加入 `load-path`，require `start`
2. `site-lisp/start.el` — require `init`
3. `site-lisp/config/init.el` — 主配置协调器：
   - **同步加载**核心模块（字体、主题、按键、tab、tray、ivy 等）
   - 通过自定义 `my-load-packages-incrementally`（0.2s 间隔）在空闲间隙**增量加载**：高亮、行号、导航、编辑工具、搜索、session 恢复等
   - **lazy-load 延迟加载**：agent-shell 配置随增量加载器加载（轻量），但 acp/agent-shell 重型包通过 `with-eval-after-load` + stub 延迟到首次调用时才加载
4. `init-const.el` — 平台检测常量（`sys/windows-p`、`sys/linux-p`、`sys/mac-p`）和目录路径
5. `init-accelerate.el` — 启动性能调优（GC 延迟、字体缓存、bidi、Windows IO 优化、gcmh）

## 代码规范

- 配置模块命名：`site-lisp/config/init-<feature>.el`
- 每个配置模块自包含：require 依赖、配置包、provide 自身
- 延迟加载使用 `lazy-load` 宏（不用 `use-package`）；重型模块（如 agent-shell）使用 `with-eval-after-load` + autoload stub 实现按需加载
- 平台判断使用 `init-const.el` 中的 `sys/windows-p`、`sys/mac-p`、`sys/linux-p`
- 根目录变量：`my-emacs-root-dir`（site-lisp/），配置目录：`my-emacs-config-dir`
- 所有 `.el` 文件使用 `lexical-binding: t` 和 `utf-8` 编码

## 错误排查

- `M-x toggle-debug-on-error` 开启错误调试，查看 backtrace 定位插件问题
- `M-x find-function <函数名>` 定位函数定义
- 使用 `benchmark-init`（init.el 中已注释）分析启动耗时
