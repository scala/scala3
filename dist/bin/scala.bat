@echo off
setlocal enabledelayedexpansion

@rem #########################################################################
@rem ## Environment setup

set _EXITCODE=0

set "_PROG_HOME=%~dp0"

call "%_PROG_HOME%\common.bat"
if not %_EXITCODE%==0 goto end

call :args %*

@rem #########################################################################
@rem ## Main

set _CASE_REPL=0
if %_EXECUTE_REPL%==1 set _CASE_REPL=1
if %_EXECUTE_RUN%==0 if %_OPTIONS_INDICATOR%==0 set _CASE_REPL=1

set _CASE_EXEC=0
if %_EXECUTE_REPL%==1 set _CASE_EXEC=1
if defined _RESIDUAL_ARGS set _CASE_EXEC=1

if %_EXECUTE_SCRIPT%==1 (
    set _SCALAC_ARGS=
    if defined _CLASS_PATH set _SCALAC_ARGS=-classpath "%_CLASS_PATH%"
    set _RESIDUAL_ARGS=!_RESIDUAL_ARGS! "-Dscript.path=%_TARGET_SCRIPT%" 
    set _SCALAC_ARGS=!_SCALAC_ARGS! %_JAVA_OPTS% !_RESIDUAL_ARGS! -script "%_TARGET_SCRIPT%" %_SCRIPT_ARGS%
    call "%_PROG_HOME%\scalac.bat" !_SCALAC_ARGS!
    if not !ERRORLEVEL!==0 ( set _EXITCODE=1& goto end )
@rem if [ $execute_repl == true ] || ([ $execute_run == false ] && [ $options_indicator == 0 ]); then
) else if %_CASE_REPL%==1 (
    set _SCALAC_ARGS=
    if defined _CLASS_PATH set _SCALAC_ARGS=-classpath "%_CLASS_PATH%"
    set _SCALAC_ARGS=!_SCALAC_ARGS! %_JAVA_OPTS% -repl %_RESIDUAL_ARGS%
    call "%_PROG_HOME%\scalac.bat" !_SCALAC_ARGS!
    if not !ERRORLEVEL!==0 ( set _EXITCODE=1& goto end )
@rem elif [ $execute_repl == true ] || [ ${#residual_args[@]} -ne 0 ]; then
) else if %_CASE_EXEC%==1 (
    set "_CP_ARG=%_SCALA3_LIB%%_PSEP%%_SCALA_LIB%"
    if defined _CLASS_PATH ( set "_CP_ARG=!_CP_ARG!%_PSEP%%_CLASS_PATH%"
    ) else ( set "_CP_ARG=!_CP_ARG!%_PSEP%."
    )
    if %_CLASS_PATH_COUNT% gtr 1 (
        echo Warning: Multiple classpaths are found, scala only use the last one. 1>&2
    )
    if %_WITH_COMPILER%==1 (
        set "_CP_ARG=!_CP_ARG!%_PSEP%%_SCALA3_COMP%%_PSEP%%_TASTY_CORE%%_PSEP%%_SCALA3_INTF%%_PSEP%%_SCALA_ASM%%_PSEP%%_SCALA3_STAGING%%_PSEP%%_SCALA3_TASTY_INSPECTOR%"
    )
    set _JAVA_ARGS=-classpath "!_CP_ARG!" %_JVM_OPTS% %_RESIDUAL_ARGS%
    call "%_JAVACMD%" !_JAVA_ARGS!
    if not !ERRORLEVEL!==0 ( set _EXITCODE=1& goto end )
) else (
    echo Warning: Command option is not correct. 1>&2
)

goto end

@rem #########################################################################
@rem ## Subroutines

:args
set _RESIDUAL_ARGS=
set _SCRIPT_ARGS=
set _EXECUTE_REPL=0
set _EXECUTE_RUN=0
set _EXECUTE_SCRIPT=0
set _TARGET_SCRIPT=
set _WITH_COMPILER=0
set _CLASS_PATH_COUNT=0
set _CLASS_PATH=
set _JVM_OPTS=
set _JAVA_OPTS=
set _OPTIONS_INDICATOR=0

:args_loop
if "%~1"=="" goto args_done
set "__ARG=%~1"
if "%__ARG%"=="-repl" (
    set _EXECUTE_REPL=1
) else if "%__ARG%"=="-run" (
    set _EXECUTE_RUN=1
) else if "%__ARG%"=="-classpath" (
    set "_CLASS_PATH=%~2"
    set /a _CLASS_PATH_COUNT+=1
    shift
) else if "%__ARG%"=="-cp" (
    set "_CLASS_PATH=%~2"
    set /a _CLASS_PATH_COUNT+=1
    shift
) else if "%__ARG%"=="-with-compiler" (
    set _WITH_COMPILER=1
) else if "%__ARG:~0,2%"=="-J" (
    set _JVM_OPTS=!_JVM_OPTS! %__ARG:~2%
    set _JAVA_OPTS=!_JAVA_OPTS! %__ARG%
) else (
    @rem _OPTIONS_INDICATOR != 0 if at least one parameter is not an option
    if not "%__ARG:~0,1%"=="-" set /a _OPTIONS_INDICATOR+=1
    if %_EXECUTE_SCRIPT%==1 (
        set _SCRIPT_ARGS=%_SCRIPT_ARGS% %__ARG%
    ) else if "%__ARG:~-6%"==".scala" (
        set _EXECUTE_SCRIPT=1
        set "_TARGET_SCRIPT=%__ARG%"
    ) else (
        set _RESIDUAL_ARGS=%_RESIDUAL_ARGS% %__ARG%
    )
)
shift
goto args_loop
:args_done
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
exit /b %_EXITCODE%
endlocal
