@echo off
if exist llama.lib del llama.lib

\run\masm.exe -mx src\stdio\printi.asm;
\run\masm.exe -mx src\stdio\readi.asm;
\run\masm.exe -mx src\stdio\printc.asm;
\run\masm.exe -mx src\stdio\readc.asm;
\run\masm.exe -mx src\stdio\printb.asm;
\run\masm.exe -mx src\stdio\readb.asm;
\run\masm.exe -mx src\stdio\printr.asm;
\run\masm.exe -mx src\stdio\readr.asm;
\run\masm.exe -mx src\stdio\prints.asm;
\run\masm.exe -mx src\stdio\reads.asm;

\run\lib.exe llama.lib /NOIGNORECASE +printc.obj +readc.obj;
\run\lib.exe llama.lib /NOIGNORECASE +printb.obj +readb.obj;
\run\lib.exe llama.lib /NOIGNORECASE +printr.obj +readr.obj;
\run\lib.exe llama.lib /NOIGNORECASE +prints.obj +reads.obj;
\run\lib.exe llama.lib /NOIGNORECASE +printi.obj +readi.obj;

\run\masm.exe -mx src\math\abs.asm;
\run\masm.exe -mx src\math\fabs.asm;
\run\masm.exe -mx src\math\sqrt.asm;
\run\masm.exe -mx src\math\sin.asm;
\run\masm.exe -mx src\math\cos.asm;
\run\masm.exe -mx src\math\tan.asm;
\run\masm.exe -mx src\math\atan.asm;
\run\masm.exe -mx src\math\exp.asm;
\run\masm.exe -mx src\math\ln.asm;masm
\run\masm.exe -mx src\math\pi.asm;
\run\masm.exe -mx src\math\pow.asm

\run\lib.exe llama.lib /NOIGNORECASE +abs.obj +fabs.obj +sqrt.obj;
\run\lib.exe llama.lib /NOIGNORECASE +sin.obj +cos.obj +tan.obj +atan.obj;
\run\lib.exe llama.lib /NOIGNORECASE +exp.obj +ln.obj +pi.obj +pow.obj;

\run\masm.exe -mx src\stdlib\incr.asm;
\run\masm.exe -mx src\stdlib\decr.asm;
\run\masm.exe -mx src\stdlib\float.asm;
\run\masm.exe -mx src\stdlib\trunc.asm;
\run\masm.exe -mx src\stdlib\round.asm;
\run\masm.exe -mx src\stdlib\ord.asm;
\run\masm.exe -mx src\stdlib\chr.asm;
\run\masm.exe -mx src\stdlib\exit.asm;

\run\lib.exe llama.lib /NOIGNORECASE +incr.obj +decr.obj;
\run\lib.exe llama.lib /NOIGNORECASE +float.obj +trunc.obj +round.obj;
\run\lib.exe llama.lib /NOIGNORECASE +ord.obj +chr.obj;
\run\lib.exe llama.lib /NOIGNORECASE +exit.obj;

\run\masm.exe -mx src\string\strlen.asm;
\run\masm.exe -mx src\string\strcmp.asm;
\run\masm.exe -mx src\string\strcpy.asm;
\run\masm.exe -mx src\string\strcat.asm;

\run\lib.exe llama.lib /NOIGNORECASE +strlen.obj +strcmp.obj;
\run\lib.exe llama.lib /NOIGNORECASE +strcpy.obj +strcat.obj;

\run\masm.exe -mx src\auxil\new.asm;
\run\masm.exe -mx src\auxil\dispose.asm;
\run\masm.exe -mx src\auxil\makearr.asm;
\run\masm.exe -mx src\auxil\formati.asm;
\run\masm.exe -mx src\auxil\formatr.asm;
\run\masm.exe -mx src\auxil\parsei.asm;
\run\masm.exe -mx src\auxil\parser.asm;
\run\masm.exe -mx src\auxil\read.asm;
\run\masm.exe -mx src\auxil\print.asm;

\run\lib.exe llama.lib /NOIGNORECASE +new.obj +dispose.obj;
\run\lib.exe llama.lib /NOIGNORECASE +makearr.obj;
\run\lib.exe llama.lib /NOIGNORECASE +formati.obj +formatr.obj;
\run\lib.exe llama.lib /NOIGNORECASE +parsei.obj +parser.obj;
\run\lib.exe llama.lib /NOIGNORECASE +print.obj +read.obj;

\run\lib.exe llama.lib /NOIGNORECASE, llama.lst;
ren llama.lib llama.lib
ren llama.lst llama.lst

del *.obj
del *.bak

