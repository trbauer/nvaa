@echo off

make --version >NUL
cl

cd mincu-tests
..\bexp mincu-tests.cu:exe:75
cd ..

cd consts
..\bexp consts.cu:exe:75
cd ..

cd copy
..\bexp copies.cu:exe:75
cd ..

cd histograms
..\bexp hist.cu:exe:75
cd ..

cd match
..\bexp match.cu:exe:75
cd ..

cd mbarriers
..\bexp mbarrier-example.cu:exe:75
cd ..

cd pipelines
..\bexp pipelines.cu:exe:75
cd ..

cd shuffles
@REM ..\bexp main.cu:exe:75
echo making shuffles
make exe
cd ..

cd stack
..\bexp stack.cu:exe:75
cd ..



cd timers
@REM ..\bexp main.cu:exe:75
echo making timers
make exe
cd ..

