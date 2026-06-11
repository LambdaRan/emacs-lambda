# emacs-lambda

**emacs-lambda**是自己使用emacs的配置文件，首次使用按以下步骤进行配置

## 首次配置
1. 下载 emacs-lambda 仓库:
```
git clone https://github.com/LambdaRan/emacs-lambda.git
```
2. 下载所有依赖包:
```
python assistant.py sync --all                        # 更新全部包
# 或者
python assistant.py sync company-mode                 # 只更新指定包
python assistant.py sync company-mode yasnippet       # 更新多个包
```
3. 将setup/site-start.el改成.emacs复制到用户目录下
4. 将setup/early-init.el拷贝到.emacs.d目录
5. 安装必要的软件
* 安装图标 `M-x all-the-icons-install-fonts`
* 安装[JetBrains Mono](https://www.jetbrains.com/zh-cn/lp/mono/#font-family)字体
* `scoop install ripgrep`
* `scoop install fd`
* 从[ctags-win32](https://github.com/universal-ctags/ctags-win32)安装windows版本[Universal Ctags](https://github.com/universal-ctags/ctags)
* 复制.ctags文件到用户目录

## 使用安装脚本
```
python setup/install.py              # 交互模式（逐步确认）
python setup/install.py --yes        # 非交互模式（全部自动确认）
python setup/install.py --dry-run    # 仅显示将要执行的操作
```

## 目录结构
```
setup/
  early-init.el      → 复制到 ~/.emacs.d/early-init.el
  site-start.el      → 复制为 ~/.emacs（入口文件，将 site-lisp/ 加入 load-path）
site-lisp/
  start.el           → 由 site-start.el 加载，require init
  config/            → init-*.el 配置模块（每个功能/包一个文件）
  extensions/        → 第三方包（由 assistant.py 从 GitHub 下载）
packages.json        → 包清单（name、repo、ref、ignores、files）
assistant.py         → 包管理器（从 GitHub Archive 同步）
```

## 报错排查
* 使用`M-x toggle-debug-on-error`来开启错误调试，当错误发生时，查看backtrace。backtrace会显示函数调用的堆栈，可能其中会包含插件相关的函数名，从而确定是哪个插件的问题。

* 使用`M-x find-function xxfunname`

---  
> 参考配置 : [lazycat-emacs](https://github.com/manateelazycat/lazycat-emacs)
