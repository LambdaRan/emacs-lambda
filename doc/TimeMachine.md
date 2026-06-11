

```
git submodule update --init --recursive

git submodule foreach git reset --hard

# 下面的命令要在WSL中运行，因为windows没有awk
curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | sudo bash
sudo apt-get install git-lfs
git submodule foreach git checkout $(git remote show origin | awk '/HEAD 分支|HEAD branch/ {split($0, a, "："); print a[2]}')
```

## 其他命令
```
git submodule add https://github.com/emacs-tree-sitter/treesit-fold.git extensions/treesit-fold

# 将每个子模块更新到最新远程提交：
# --recursive 递归更新所有子模块中的子模块
# --remote 将子模块更新到远程仓库的最新提交（基于 .gitmodules 中指定的分支）

git submodule update --remote --recursive


```
