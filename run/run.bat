@echo off

masm /Mx %1.asm;
if errorlevel 1 goto quit

link /noignorecase %1.obj,%1.exe,nul,llama.lib;
if errorlevel 1 goto quitwobj

call %1.exe
del %1.exe

:quitwobj
del %1.obj

:quit
