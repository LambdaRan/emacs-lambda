@REM comit

@ECHO git clone emacs-lambda ...
git clone https://github.com/LambdaRan/emacs-lambda.git

@REM 延时2s
@TIMEOUT /T 2
cd emacs-lambda

@ECHO update ...
git submodule update --init --recursive

git submodule foreach git reset --hard
git submodule foreach git checkout master

@REM 按任意键退出
pause