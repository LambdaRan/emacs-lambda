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

git submodule foreach git checkout master
```

## 编译Emacs 支持Xwidgets
> https://github.com/daviderestivo/homebrew-emacs-head

> https://github.crookster.org/emacs27-from-homebrew-on-macos-with-emoji/

安装命令：
```bash
brew tap daviderestivo/emacs-head
brew install emacs-head --HEAD --without-imagemagick@7 --with-cocoa --with-xwidgets
ln -s /usr/local/opt/emacs-head/Emacs.app /Applications

# 或者
brew tap d12frosted/emacs-plus
brew install emacs-plus --HEAD --without-spacemacs-icon --with-jansson --with-xwidgets
```

在编译时，会从`https://raw.githubusercontent.com/xxx`下载文件，此过程会频繁出现问题，如：
* curl: (35) LibreSSL SSL_connect: SSL_ERROR_SYSCALL in connection to raw.githubusercontent.com:443
* curl (7) failed to connect to raw.githubusercontent.com port 443 connection refused

...

等等，一系列问题并且下载还非常非常慢！

解决方法：
1. 查看brew使用的formula，以emacs-head为例
```bash
brew info emacs-head
daviderestivo/emacs-head/emacs-head@27: stable 27.2, HEAD
GNU Emacs text editor
https://www.gnu.org/software/emacs/
/usr/local/Cellar/emacs-head@27/HEAD-2b7eed2_1 (4,017 files, 131.5MB) *
  Built from source on 2021-04-18 at 14:26:02 with: --without-imagemagick@7 --with-cocoa --with-xwidgets
From: https://github.com/daviderestivo/homebrew-emacs-head/blob/HEAD/Formula/emacs-head@27.rb
```
2. 下载仓库homebrew-emacs-head
```bash
git clone https://github.com/daviderestivo/homebrew-emacs-head.git ~/test
```
3. 在仓库homebrew-emacs-head存储目录下，利用Python起一个webserver
```bash
cd ~/test
python -m SimpleHTTPServer 8765
# ifconfig/hostname -i 记下自己的IP地址
```
4. 拷贝并修改brew formula文件
```bash
cd ~/test
cp homebrew-emacs-head/Formula/emacs-head@27.rb ./
vim emacs-head@27.rb
```
```ruby
# 把资源获取地址 https://raw.githubusercontent.com/daviderestivo/homebrew-emacs-head
# 改成自己本地启动的server地址 xxx:8765/homebrew-emacs-head

  def self.get_resource_url(resource)
    if ENV['HOMEBREW_GITHUB_REF']
      branch = ENV['HOMEBREW_GITHUB_REF'].sub("refs/heads/", "")
      "https://raw.githubusercontent.com/daviderestivo/homebrew-emacs-head/" + branch +  "/" + resource
    else
      "192.168.11.1:8765/homebrew-emacs-head/" + resource
    end
  end
```
5. 通过修改后的formula文件下载
```bash
brew install ~/test/emacs-head@27.rb --HEAD --without-imagemagick@7 --with-cocoa --with-xwidgets
```
