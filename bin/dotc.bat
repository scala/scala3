@echo off
setlocal

for %%f in ("%~dp0..") do set _ROOT_DIR=%%~sf

call %_ROOT_DIR%\bin\common.bat "%_ROOT_DIR%\dist-bootstrapped\target\pack\bin\dotc.bat" %*
