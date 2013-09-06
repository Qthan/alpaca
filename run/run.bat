@echo off

masm /Mx /t %1.asm;
if errorlevel 1 goto quit

link /tiny /noignorecase %1.obj,%1.com,nul,llama.lib;
if errorlevel 1 goto quitwobj

call %1.com
del %1.com

:quitwobj
del %1.obj

:quit
