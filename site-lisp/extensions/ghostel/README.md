# Ghostel
> [emcas ghostel](https://github.com/dakra/ghostel)
> [ghostel](https://github.com/ghostty-org/ghostty)

emacs ghostel 类似emacs-libvterm，是基于[libghostty-vt](https://ghostty.org/) 开发的Emacs 终端模拟器.

当前项目是通过[conpty-proxy](https://github.com/xhcoding/conpty-proxy)的方式,让windows下的emacs支持ghostel.

## 配置步骤
1. 克隆[conpty-ghostel](https://github.com/LambdaRan/conpty_ghostel),进入目录执行`build.cmd`脚本进行编译
2. 将`ghostel.el`,编译产出`ghostel-module.dll`和`conpty_proxy.exe`放到自己emacs的配置目录
3. 配置`ghostel`,启动emac后,`M-x ghostel`启动一个终端.


