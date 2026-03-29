# vterm


### 配置步骤

1. 下载 [conpty_proxy.exe](https://github.com/xhcoding/conpty-proxy)
   
   放到想放的位置，可通过`vterm-conpty-proxy-path`自定义位置。vterm启动时会在同目录查找。
2. 下载修改后[vterm.el](https://github.com/xhcoding/emacs-libvterm/tree/master), 放到自己的emacs配置中
   
   原仓库[emacs-libvterm](https://github.com/akermu/emacs-libvterm)
   
3. 下载对应的[dll](https://github.com/LambdaRan/emacs-libvterm/actions), 有ucrt和普通的版本可选，

    有ucrt和普通的版本可选，fork是因为可以使用github的action来生成，不用本地搭环境。我使用的是普通版本。
    
    解压后有libvterm-0.dll 和 vterm-module.dll两个文件。libvterm-0.dll文件要放到emacs的bin目录下。
    

### 使用

M-x  vterm 就可以

体验有点慢，但临时用用还是可以的。


