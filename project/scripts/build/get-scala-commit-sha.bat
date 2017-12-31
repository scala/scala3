@echo off
rem See more documentation in the corresponding Shell script.

if not [%1]==[] cd /d %1

for /f "delims=" %%s in ('git log -1 --format^=""%%H"" HEAD') do set hash=%%s
rem If some errors happen; e.g. Git is not installed.
if not defined hash exit 1
echo %hash%
