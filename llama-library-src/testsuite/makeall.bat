@echo off
..\support\masm /Mx math.asm;
..\support\link /tiny /noignorecase math.obj,math.com,nul,..\llama.lib;
..\support\masm /Mx stdio.asm;
..\support\link /tiny /noignorecase stdio.obj,stdio.com,nul,..\llama.lib;
..\support\masm /Mx string.asm;
..\support\link /tiny /noignorecase string.obj,string.com,nul,..\llama.lib;
..\support\masm /Mx mkarr.asm;
..\support\link /tiny /noignorecase mkarr.obj,mkarr.com,nul,..\llama.lib;
