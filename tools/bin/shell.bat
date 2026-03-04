@echo off

if not defined VSCMD_VER (
    set VSCMD_SKIP_SENDTELEMETRY=1
    call "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
)

set path=w:\handmade\misc;%path%

