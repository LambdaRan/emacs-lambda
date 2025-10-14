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

# 下面的命令要在WSL中运行，因为windows没有awk
curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | sudo bash
sudo apt-get install git-lfs
git submodule foreach git checkout $(git remote show origin | awk '/HEAD 分支|HEAD branch/ {split($0, a, "："); print a[2]}')

```

3. 将site-start.el改成.emacs复制到用户目录下

4. 安装必要的软件

* 安装图标 `M-x all-the-icons-install-fonts`
* 安装[JetBrains Mono](https://www.jetbrains.com/zh-cn/lp/mono/#font-family)字体
* `scoop install ripgrep`
* `scoop install fd`
* 从[ctags-win32](https://github.com/universal-ctags/ctags-win32)安装windows版本[Universal Ctags](https://github.com/universal-ctags/ctags)
* 复制.ctags文件到用户目录

## 其他命令
```
git submodule add https://github.com/emacs-tree-sitter/treesit-fold.git extensions/treesit-fold

# 将每个子模块更新到最新远程提交：
# --recursive 递归更新所有子模块中的子模块
# --remote 将子模块更新到远程仓库的最新提交（基于 .gitmodules 中指定的分支）

git submodule update --remote --recursive


```

## 报错排查

  使用`M-x toggle-debug-on-error`来开启错误调试，当错误发生时，查看backtrace。backtrace会显示函数调用的堆栈，可能其中会包含插件相关的函数名，从而确定是哪个插件的问题。
  
  使用`M-x find-function xxfunname`
  

