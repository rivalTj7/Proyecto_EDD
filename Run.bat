REM
mkdir bin
REM 
del bin\main.exe
REM 
gfortran -g -o bin\main.exe main.f90 Records/Record.f90
REM Run the executable
bin\main.exe