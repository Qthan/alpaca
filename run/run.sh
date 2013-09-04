#!/usr/bin/zsh


# run me with a single argument, the main name of the .asm file
# mind the 8.3 rule for file names under DOS: the main filename must be up to 8 characters long
# make sure the .asm file is inside the current folder, as well as run.bat, grace.lib, masm.exe 
# and link.exe

dosbox -c "mount C ." -c "C:" -c "run.bat $1"
