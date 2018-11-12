@echo off
setlocal enabledelayedexpansion

rem only for interactive debugging !
set _DEBUG=0

rem ##########################################################################
rem ## Environment setup

set _EXITCODE=0

set _BASENAME=%~n0

for %%f in ("%~dp0..") do set _PROG_HOME=%%~sf

call %_PROG_HOME%\bin\common.bat
if not %_EXITCODE%==0 goto end

call :args %*

rem ##########################################################################
rem ## Main

set _CASE_1=0
if %_EXECUTE_REPL%==1 set _CASE_1=1
if %_EXECUTE_RUN%==0 if not defined _RESIDUAL_ARGS set _CASE_1=1

set _CASE_2=0
if %_EXECUTE_RUN%==1 set _CASE_2=1
if defined _RESIDUAL_ARGS set _CASE_2=1

rem if [ $execute_repl == true ] || ([ $execute_run == false ] && [ $options_indicator == 0 ]); then
if %_CASE_1%==1 (
    set _DOTC_ARGS=
    if defined _CLASS_PATH set _DOTC_ARGS=-classpath "%_CLASS_PATH%"
    set _DOTC_ARGS=!_DOTC_ARGS! %_JAVA_OPTIONS% -repl %_RESIDUAL_ARGS%
    echo Starting dotty REPL...
    if %_DEBUG%==1 echo [%_BASENAME%] %_PROG_HOME%\bin\dotc.bat !_DOTC_ARGS!
    %_PROG_HOME%\bin\dotc.bat !_DOTC_ARGS!
rem elif [ $execute_repl == true ] || [ ${#residual_args[@]} -ne 0 ]; then
) else if %_CASE_2%==1 (
    set _CP_ARG=%_DOTTY_LIB%%_PSEP%%_SCALA_LIB%
    if defined _CLASS_PATH ( set _CP_ARG=!_CP_ARG!%_PSEP%%_CLASS_PATH%
    ) else ( set _CP_ARG=!_CP_ARG!%_PSEP%.
    )
    if %_CLASS_PATH_COUNT% gtr 1 (
        echo warning: multiple classpaths are found, dotr only use the last one.
    )
    if %_WITH_COMPILER%==1 (
        set _CP_ARG=!_CP_ARG!%_PSEP%%_DOTTY_COMP%%_PSEP%%_DOTTY_INTF%%_PSEP%%_SCALA_ASM%
    )
    set _JAVA_ARGS=%_JAVA_DEBUG% -classpath "!_CP_ARG!" %_RESIDUAL_ARGS%
    if %_DEBUG%==1 echo [%_BASENAME%] %_JAVACMD% !_JAVA_ARGS!
    %_JAVACMD% !_JAVA_ARGS!
) else (
    echo warning: command option is not correct.
)

goto end

rem ##########################################################################
rem ## Subroutines

:args
set _RESIDUAL_ARGS=
set _EXECUTE_REPL=0
set _EXECUTE_RUN=0
set _WITH_COMPILER=0
set _JAVA_DEBUG=
set _CLASS_PATH_COUNT=0
set _CLASS_PATH=
set _JVM_OPTIONS=
set _JAVA_OPTIONS=

:args_loop
if "%1"=="" goto args_done
set "_ARG=%1"
if %_DEBUG%==1 echo [%_BASENAME%] _ARG=%_ARG%
if /i "%_ARG%"=="-repl" (
    set _EXECUTE_REPL=1
) else if /i "%_ARG%"=="-run" (
    set _EXECUTE_RUN=1
) else if /i "%_ARG%"=="-classpath" (
    set _CLASS_PATH=%2
    set /a _CLASS_PATH_COUNT+=1
    shift
) else if /i "%_ARG%"=="-with-compiler" (
    set _WITH_COMPILER=1
) else if /i "%_ARG%"=="-d" (
    set _JAVA_DEBUG=%_DEBUG_STR%
) else if /i "%_ARG:~0,2%"=="-J" (
    set _JVM_OPTIONS=!_JVM_OPTIONS! %_ARG%
    set _JAVA_OPTIONS=!_JAVA_OPTIONS! %_ARG%
) else (
    set _RESIDUAL_ARGS=%_RESIDUAL_ARGS% %_ARG%
)
shift
goto args_loop
:args_done
goto :eof

rem ##########################################################################
rem ## Cleanups

:end
if %_DEBUG%==1 echo [%_BASENAME%] _EXITCODE=%_EXITCODE%
exit /b %_EXITCODE%
endlocal
