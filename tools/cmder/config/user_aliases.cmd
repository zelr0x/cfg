;= @echo off
;= rem Call DOSKEY and use this file as the macrofile
;= %SystemRoot%\system32\doskey /listsize=1000 /macrofile=%0%
;= rem In batch mode, jump to the end of the file
;= goto:eof
;= Add aliases below here
e.=explorer .
gl=git log --oneline --all --graph --decorate  $*
ls=ls --show-control-chars -F --color $*
l=ls -lAshF $*
pwd=cd
clear=cls
unalias=alias /d $1
vi=vim $*
cmderr=cd /d "%CMDER_ROOT%"
pwsh=%SystemRoot%/System32/WindowsPowerShell/v1.0/powershell.exe -ExecutionPolicy Bypass -NoLogo -NoProfile -NoExit -Command "Invoke-Expression '. ''%CMDER_ROOT%/vendor/profile.ps1'''"
#mp3dl=youtube-dl --verbose -x --audio-format mp3 --flat-playlist --no-playlist -o "C:\all\audio\dl\%(title)s.%(ext)s" $*
mp3dl=yt-dlp --verbose -x --audio-format mp3 --flat-playlist --no-playlist -o "C:\all\audio\dl\%(title)s.%(ext)s" $*
emacss=emacs $*
emacst=emacs -nw $*
emacs=runemacs $*

