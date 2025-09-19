## ����Emacs ֧��Xwidgets
> https://github.com/daviderestivo/homebrew-emacs-head

> https://github.crookster.org/emacs27-from-homebrew-on-macos-with-emoji/

��װ���
```bash
brew tap daviderestivo/emacs-head
brew install emacs-head --HEAD --without-imagemagick@7 --with-cocoa --with-xwidgets
ln -s /usr/local/opt/emacs-head/Emacs.app /Applications

# ����
brew tap d12frosted/emacs-plus
brew install emacs-plus --HEAD --without-spacemacs-icon --with-jansson --with-xwidgets
```

�ڱ���ʱ�����`https://raw.githubusercontent.com/xxx`�����ļ����˹��̻�Ƶ���������⣬�磺
* curl: (35) LibreSSL SSL_connect: SSL_ERROR_SYSCALL in connection to raw.githubusercontent.com:443
* curl (7) failed to connect to raw.githubusercontent.com port 443 connection refused

...

�ȵȣ�һϵ�����Ⲣ�����ػ��ǳ��ǳ�����

���������
1. �鿴brewʹ�õ�formula����emacs-headΪ��
```bash
brew info emacs-head
daviderestivo/emacs-head/emacs-head@27: stable 27.2, HEAD
GNU Emacs text editor
https://www.gnu.org/software/emacs/
/usr/local/Cellar/emacs-head@27/HEAD-2b7eed2_1 (4,017 files, 131.5MB) *
  Built from source on 2021-04-18 at 14:26:02 with: --without-imagemagick@7 --with-cocoa --with-xwidgets
From: https://github.com/daviderestivo/homebrew-emacs-head/blob/HEAD/Formula/emacs-head@27.rb
```
2. ���زֿ�homebrew-emacs-head
```bash
git clone https://github.com/daviderestivo/homebrew-emacs-head.git ~/test
```
3. �ڲֿ�homebrew-emacs-head�洢Ŀ¼�£�����Python��һ��webserver
```bash
cd ~/test
python -m SimpleHTTPServer 8765
# ifconfig/hostname -i �����Լ���IP��ַ
```
4. �������޸�brew formula�ļ�
```bash
cd ~/test
cp homebrew-emacs-head/Formula/emacs-head@27.rb ./
vim emacs-head@27.rb
```
```ruby
# ����Դ��ȡ��ַ https://raw.githubusercontent.com/daviderestivo/homebrew-emacs-head
# �ĳ��Լ�����������server��ַ xxx:8765/homebrew-emacs-head

  def self.get_resource_url(resource)
    if ENV['HOMEBREW_GITHUB_REF']
      branch = ENV['HOMEBREW_GITHUB_REF'].sub("refs/heads/", "")
      "https://raw.githubusercontent.com/daviderestivo/homebrew-emacs-head/" + branch +  "/" + resource
    else
      "192.168.11.1:8765/homebrew-emacs-head/" + resource
    end
  end
```
5. ͨ���޸ĺ��formula�ļ�����
```bash
brew install ~/test/emacs-head@27.rb --HEAD --without-imagemagick@7 --with-cocoa --with-xwidgets
```
