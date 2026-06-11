
# assistant.py 命令参考

## sync — 同步依赖包

```bash
python assistant.py sync --all                        # 更新全部包
python assistant.py sync --all --clean                # 更新全部包并清理临时目录
python assistant.py sync company-mode                 # 只更新指定包
python assistant.py sync company-mode yasnippet       # 更新多个包
```

同步时下载的临时文件存放在项目同级目录 `{project}_temp/`，默认保留不删除。加 `--clean` 参数在同步完成后自动删除。

## add — 添加新依赖包

```bash
# 添加包（name 自动从 repo 推导）
python assistant.py add company-mode/company-mode

# 指定分支
python assistant.py add company-mode/company-mode --ref main

# 指定包名
python assistant.py add company-mode/company-mode --name company

# 添加后立即下载
python assistant.py add company-mode/company-mode --sync

# 指定 files
python assistant.py add someone/pkg --files "pkg.el" "pkg-utils.el"

# 指定额外排除
python assistant.py add someone/pkg --ignores "examples/" "benchmarks/"

# 添加手动维护包
python assistant.py add someone/pkg --manual --note "手动维护"
```

---

# packages.json 配置说明

`packages.json` 是 Emacs 依赖包的声明文件，放在仓库根目录。`assistant.py` 脚本根据此文件从 GitHub 下载和管理依赖包。

## 整体结构

```json
{
  "packages": [
    { ... },
    { ... }
  ]
}
```

顶层是一个对象，包含 `packages` 数组，每个元素描述一个依赖包。

---

## 字段说明

### 必填字段

| 字段 | 类型 | 说明 |
|---|---|---|
| `name` | string | 包名，对应 `site-lisp/extensions/{name}/` 目录 |
| `repo` | string | GitHub 仓库地址，格式为 `owner/repo`。`manual` 包可省略 |
| `ref` | string | 跟踪的分支名，如 `master`、`main`。`manual` 包可省略 |

### 可选字段

| 字段 | 类型 | 说明 |
|---|---|---|
| `files` | string[] | 要拷贝的文件列表，支持 glob 通配符。不指定则全量拷贝（按排除规则过滤） |
| `ignores` | string[] | 额外的排除规则，叠加在全局默认排除之上。仅在未指定 `files` 时生效 |
| `manual` | bool | 设为 `true` 时，sync 命令跳过此包，不做任何操作 |
| `note` | string | 备注说明，记录包的用途或特殊注意事项 |

---

## files 字段

当包中有很多文件但你只需要其中一部分时，使用 `files` 字段精确控制拷贝范围。

### 支持的通配符

| 模式 | 含义 | 示例 |
|---|---|---|
| `*.el` | 当前目录所有 .el 文件 | 匹配 `foo.el`、`bar.el` |
| `prefix*.el` | 匹配前缀 | `treesit-fold*.el` 匹配 `treesit-fold.el`、`treesit-fold-parsers.el` |
| `dir/*.el` | 子目录下所有 .el 文件 | `lisp/*.el` |
| `dir/**/*` | 递归匹配所有文件 | `snippets/**/*` |
| `dir/` | 整个目录递归拷贝 | `icons/` |

### 示例

```json
{
  "name": "yasnippet",
  "repo": "joaotavora/yasnippet",
  "ref": "master",
  "files": ["yasnippet.el"]
}
```

只拷贝 `yasnippet.el`，忽略其他所有文件。

> **注意**：指定 `files` 后，`ignores` 字段不生效，因为 `files` 已经是精确控制。

---

## ignores 字段

当未指定 `files`（即全量拷贝模式）时，`ignores` 用于排除不需要的文件或目录。它在全局默认排除规则的基础上**叠加**生效，只需写额外的排除项。

### 全局默认排除规则

以下规则对所有包自动生效，无需在 `ignores` 中重复：

```
test*          tests
doc            docs
.github        .travis*         .circleci
Makefile       Cask             Eask
CONTRIBUTING*  CHANGELOG*       NEWS*
.gitignore     .dir-locals.el   .elpaignore
*.elc          *.gif            *.png
```

### 示例

```json
{
  "name": "some-pkg",
  "repo": "someone/some-pkg",
  "ref": "main",
  "ignores": ["examples/", "benchmarks/", "demo.el"]
}
```

实际排除 = 全局默认 + `examples/` + `benchmarks/` + `demo.el`。

---

## manual 字段

标记为手动维护的包，sync 命令会完全跳过，不下载、不清空、不修改目录内容。

适用于：
- 包含编译后二进制文件的包（如 `.dll`、`.exe`）
- 个人工具集合
- 从非 GitHub 来源获取的包

### 示例

```json
{
  "name": "ghostel",
  "manual": true,
  "note": "手动维护，含 native dll"
}
```

---

## 完整示例

```json
{
  "packages": [
    {
      "name": "company-mode",
      "repo": "company-mode/company-mode",
      "ref": "master",
      "note": "代码补全框架"
    },
    {
      "name": "my-lib",
      "repo": "someone/my-lib",
      "ref": "main",
      "files": ["my-lib.el", "my-lib-utils.el"],
      "note": "只使用核心和工具两个文件"
    },
    {
      "name": "some-pkg",
      "repo": "someone/some-pkg",
      "ref": "main",
      "ignores": ["examples/", "benchmarks/"],
      "note": "排除示例和性能测试"
    },
    {
      "name": "ghostel",
      "manual": true,
      "note": "手动维护，含 native dll"
    },
    {
      "name": "vterm",
      "manual": true,
      "note": "手动维护，含 native dll"
    },
    {
      "name": "lazycat",
      "manual": true,
      "note": "个人工具集合"
    },
    {
      "name": "lazycat-theme",
      "manual": true,
      "note": "个人主题包"
    },
    {
      "name": "ran",
      "manual": true,
      "note": "个人工具集合"
    }
  ]
}
```


