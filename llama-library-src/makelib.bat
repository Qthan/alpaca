@echo off
if exist llama.lib del llama.lib

..\masm611\bin\masm.exe -mx \llama-library-src\src\stdio\printi.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdio\readi.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdio\printc.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdio\readc.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdio\printb.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdio\readb.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdio\printr.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdio\readr.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdio\prints.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdio\reads.asm;

masm611\binr\lib.exe llama.lib /NOIGNORECASE +printc.obj +readc.obj;
masm611\binr\lib.exe llama.lib /NOIGNORECASE +printb.obj +readb.obj;
masm611\binr\lib.exe llama.lib /NOIGNORECASE +printr.obj +readr.obj;
masm611\binr\lib.exe llama.lib /NOIGNORECASE +prints.obj +reads.obj;
masm611\binr\lib.exe llama.lib /NOIGNORECASE +printi.obj +readi.obj;

..\masm611\bin\masm.exe -mx \llama-library-src\src\math\abs.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\math\fabs.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\math\sqrt.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\math\sin.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\math\cos.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\math\tan.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\math\atan.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\math\exp.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\math\ln.asm;masm
..\masm611\bin\masm.exe -mx \llama-library-src\src\math\pi.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\math\pow.asm

masm611\binr\lib.exe llama.lib /NOIGNORECASE +abs.obj +fabs.obj +sqrt.obj;
masm611\binr\lib.exe llama.lib /NOIGNORECASE +sin.obj +cos.obj +tan.obj +atan.obj;
masm611\binr\lib.exe llama.lib /NOIGNORECASE +exp.obj +ln.obj +pi.obj +pow.obj;

..\masm611\bin\masm.exe -mx \llama-library-src\src\stdlib\incr.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdlib\decr.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdlib\float.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdlib\trunc.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdlib\round.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdlib\ord.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdlib\chr.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\stdlib\exit.asm;

masm611\binr\lib.exe llama.lib /NOIGNORECASE +incr.obj +decr.obj;
masm611\binr\lib.exe llama.lib /NOIGNORECASE +float.obj +trunc.obj +round.obj;
masm611\binr\lib.exe llama.lib /NOIGNORECASE +ord.obj +chr.obj;
masm611\binr\lib.exe llama.lib /NOIGNORECASE +exit.obj;

..\masm611\bin\masm.exe -mx \llama-library-src\src\string\strlen.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\string\strcmp.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\string\strcpy.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\string\strcat.asm;

masm611\binr\lib.exe llama.lib /NOIGNORECASE +strlen.obj +strcmp.obj;
masm611\binr\lib.exe llama.lib /NOIGNORECASE +strcpy.obj +strcat.obj;

..\masm611\bin\masm.exe -mx \llama-library-src\src\auxil\new.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\auxil\dispose.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\auxil\makearr.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\auxil\formati.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\auxil\formatr.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\auxil\parsei.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\auxil\parser.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\auxil\read.asm;
..\masm611\bin\masm.exe -mx \llama-library-src\src\auxil\print.asm;

masm611\binr\lib.exe llama.lib /NOIGNORECASE +new.obj +dispose.obj;
masm611\binr\lib.exe llama.lib /NOIGNORECASE +makearr.obj;
masm611\binr\lib.exe llama.lib /NOIGNORECASE +formati.obj +formatr.obj;
masm611\binr\lib.exe llama.lib /NOIGNORECASE +parsei.obj +parser.obj;
masm611\binr\lib.exe llama.lib /NOIGNORECASE +print.obj +read.obj;

masm611\binr\lib.exe llama.lib /NOIGNORECASE, llama.lst;
ren llama.lib llama.lib
ren llama.lst llama.lst

del *.obj
del *.bak

