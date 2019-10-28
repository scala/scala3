@echo off

rem ##########################################################################
rem ## This batch file is based on shell script project/scripts/cmdTests

setlocal enabledelayedexpansion

rem only for interactive debugging
set _DEBUG=0

rem ##########################################################################
rem ## Environment setup

set _BASENAME=%~n0

set _EXITCODE=0

for %%f in ("%~dp0..\..") do set _ROOT_DIR=%%~sf
set _SCRIPTS_DIR=%_ROOT_DIR%\project\scripts

if not defined __COMMON__ (
    if %_DEBUG%==1 echo [%_BASENAME%] call %_SCRIPTS_DIR%\common.bat 1>&2
    call %_SCRIPTS_DIR%\common.bat
    if not !_EXITCODE!==0 goto end
)

rem ##########################################################################
rem ## Main

rem # check that `sbt dotc` compiles and `sbt dotr` runs it
echo testing sbt dotc and dotr
if %_DEBUG%==1 echo [%_BASENAME%] "%_SBT_CMD%" ";dotc %_SOURCE% -d %_OUT_DIR% ;dotr -classpath %_OUT_DIR% %_MAIN%" ^> "%_TMP_FILE%" 1>&2
call "%_SBT_CMD%" ";dotc %_SOURCE% -d %_OUT_DIR% ;dotr -classpath %_OUT_DIR% %_MAIN%" > "%_TMP_FILE%"
if not %ERRORLEVEL%==0 ( set _EXITCODE=1& goto end )
call :grep "%_EXPECTED_OUTPUT%" "%_TMP_FILE%"
if not %_EXITCODE%==0 goto end

rem # check that `sbt dotc` compiles and `sbt dotr` runs it
echo testing sbt dotc -from-tasty and dotr -classpath
call :clear_out "%_OUT_DIR%"
if %_DEBUG%==1 echo [%_BASENAME%] "%_SBT_CMD%" ";dotc %_SOURCE% -d %_OUT_DIR% ;dotc -from-tasty -classpath %_OUT_DIR% -d %_OUT1_DIR% %_MAIN% ;dotr -classpath %_OUT1_DIR% %_MAIN%" ^> "%_TMP_FILE%" 1>&2
call "%_SBT_CMD%" ";dotc %_SOURCE% -d %_OUT_DIR% ;dotc -from-tasty -classpath %_OUT_DIR% -d %_OUT1_DIR% %_MAIN% ;dotr -classpath %_OUT1_DIR% %_MAIN%" > "%_TMP_FILE%"
if not %ERRORLEVEL%==0 ( set _EXITCODE=1& goto end )
call :grep "%_EXPECTED_OUTPUT%" "%_TMP_FILE%"
if not %_EXITCODE%==0 goto end

rem # check that `sbt dotc -decompile` runs
echo testing sbt dotc -decompile
if %_DEBUG%==1 echo [%_BASENAME%] "%_SBT_CMD%" ";dotc -decompile -color:never -classpath %_OUT_DIR% %_MAIN%" ^> "%_TMP_FILE%" 1>&2
call "%_SBT_CMD%" ";dotc -decompile -color:never -classpath %_OUT_DIR% %_MAIN%" > "%_TMP_FILE%"
if not %ERRORLEVEL%==0 ( set _EXITCODE=1& goto end )
call :grep "def main(args: scala.Array\[scala.Predef.String\]): scala.Unit =" "%_TMP_FILE%"
if not %_EXITCODE%==0 goto end

echo testing sbt dotc -decompile from file
if %_DEBUG%==1 echo [%_BASENAME%] "%_SBT_CMD%" ";dotc -decompile -color:never -classpath %_OUT_DIR% %_OUT_DIR%\%_TASTY%" ^> "%_TMP_FILE%" 1>&2
call "%_SBT_CMD%" ";dotc -decompile -color:never -classpath %_OUT_DIR% %_OUT_DIR%\%_TASTY%" > "%_TMP_FILE%"
if not %ERRORLEVEL%==0 ( set _EXITCODE=1& goto end )
call :grep "def main(args: scala.Array\[scala.Predef.String\]): scala.Unit =" "%_TMP_FILE%"
if not %_EXITCODE%==0 goto end

echo testing sbt dotr with no -classpath
call :clear_out "%_OUT_DIR%"
if %_DEBUG%==1 echo [%_BASENAME%] "%_SBT_CMD%" ";dotc %_SOURCE% ; dotr %_MAIN%" ^> "%_TMP_FILE%" 1>&2
call "%_SBT_CMD%" ";dotc %_SOURCE% ; dotr %_MAIN%" > "%_TMP_FILE%"
if not %ERRORLEVEL%==0 ( set _EXITCODE=1& goto end )
call :grep "%_EXPECTED_OUTPUT%" "%_TMP_FILE%"
if not %_EXITCODE%==0 goto end

echo testing loading tasty from .tasty file in jar
call :clear_out "%_OUT_DIR%"
if %_DEBUG%==1 echo [%_BASENAME%] "%_SBT_CMD%" ";dotc -d %_OUT_DIR%\out.jar %_SOURCE%; dotc -decompile -classpath %_OUT_DIR%\out.jar -color:never %_MAIN%" ^> "%_TMP_FILE%" 1>&2
call "%_SBT_CMD%" ";dotc -d %_OUT_DIR%\out.jar %_SOURCE%; dotc -decompile -classpath %_OUT_DIR%\out.jar -color:never %_MAIN%" > "%_TMP_FILE%"
if not %ERRORLEVEL%==0 ( set _EXITCODE=1& goto end )
call :grep "def main(args: scala.Array\[scala.Predef.String\]): scala.Unit =" "%_TMP_FILE%"
if not %_EXITCODE%==0 goto end

goto end

rem ##########################################################################
rem ## Subroutines

:clear_out
set __OUT_DIR=%~1

if exist "%__OUT_DIR%\" (
    if %_DEBUG%==1 echo [%_BASENAME%] del /s /q "%__OUT_DIR%\*" 1>&2
    del /s /q "%__OUT_DIR%\*" 1>NUL
)
goto :eof

:grep 
set __PATTERN=%~1
set __FILE=%~2

if %_DEBUG%==1 echo [%_BASENAME%] findstr "%__PATTERN%" "%__FILE%" 1>&2
findstr "%__PATTERN%" "%__FILE%"
if not %ERRORLEVEL%==0 (
    echo Error: Failed to find pattern "%__PATTERN%" in file %__FILE% 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

rem ##########################################################################
rem ## Cleanups

:end
if %_DEBUG%==1 echo [%_BASENAME%] _EXITCODE=%_EXITCODE% 1>&2
exit /b %_EXITCODE%
endlocal


