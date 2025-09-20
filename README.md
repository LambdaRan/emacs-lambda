# emacs-lambda
### Origin : [lazycat-emacs](https://github.com/manateelazycat/lazycat-emacs)

## 获取配置
1. 下载 emacs-lambda 仓库:
```
git clone https://github.com/LambdaRan/emacs-lambda.git
```

2. 下载所有子模块:
```
git submodule update --init --recursive

git submodule foreach git reset --hard

git submodule foreach git checkout $(git remote show origin | awk '/HEAD 分支|HEAD branch/ {split($0, a, "："); print a[2]}')
```

3. 将site-start.el改成.emacs复制到用户目录下

## 添加模块
```
git submodule add https://github.com/emacs-tree-sitter/treesit-fold.git extensions/treesit-fold
```

4. 安装必要的软件

* 安装图标 `M-x all-the-icons-install-fonts`
* 安装[JetBrains Mono](https://www.jetbrains.com/zh-cn/lp/mono/#font-family)字体
* `scoop install ripgrep`
* `scoop install fd`
* 从[ctags-win32](https://github.com/universal-ctags/ctags-win32)安装windows版本[Universal Ctags](https://github.com/universal-ctags/ctags)
* 复制.ctags文件到用户目录
