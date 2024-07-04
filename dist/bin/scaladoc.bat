@echo off
setlocal enabledelayedexpansion

@rem #########################################################################
@rem ## Environment setup

set _EXITCODE=0

for %%f in ("%~dp0.") do (
    set "_PROG_HOME=%%~dpf"
    @rem get rid of the trailing slash
    set "_PROG_HOME=!_PROG_HOME:~0,-1!"
)
call "%_PROG_HOME%\bin\common.bat"
if not %_EXITCODE%==0 goto end

set _DEFAULT_JAVA_OPTS=-Xmx768m -Xms768m

call :args %*

@rem #########################################################################
@rem ## Main

call :classpathArgs

if defined JAVA_OPTS ( set _JAVA_OPTS=%JAVA_OPTS%
) else ( set _JAVA_OPTS=%_DEFAULT_JAVA_OPTS%
)

@rem we need to escape % in the java command path, for some reason this doesnt work in common.bat
set "_JAVACMD=!_JAVACMD:%%=%%%%!"

call "%_JAVACMD%" %_JAVA_OPTS% %_JAVA_DEBUG% %_JAVA_ARGS% ^
-classpath "@%_CLASS_PATH_FILE%" ^
-Dscala.usejavacp=true ^
dotty.tools.scaladoc.Main %_SCALA_ARGS% %_RESIDUAL_ARGS%
if not %ERRORLEVEL%==0 (
    @rem echo Error: Scaladoc execution failed 1>&2
    set _EXITCODE=1
    goto end
)
goto end

@rem #########################################################################
@rem ## Subroutines

:args
set _JAVA_DEBUG=
set _HELP=
set _VERBOSE=
set _QUIET=
set _COLORS=
set _SCALA_ARGS=
set _JAVA_ARGS=
set _RESIDUAL_ARGS=

:args_loop
if "%~1"=="" goto args_done
set "__ARG=%~1"
if "%__ARG%"=="--" (
    @rem for arg; do addResidual "$arg"; done; set -- ;;
) else if "%__ARG%"=="-h" (
    set _HELP=true
    call :addScala "-help"
) else if "%__ARG%"=="-help" (
    set _HELP=true
    call :addScala "-help"
) else if "%__ARG%"=="-v" (
    set _VERBOSE=true
    call :addScala "-verbose"
) else if "%__ARG%"=="-verbose" (
    set _VERBOSE=true
    call :addScala "-verbose"
) else if "%__ARG%"=="-debug" ( set "_JAVA_DEBUG=%_DEBUG_STR%"
) else if "%__ARG%"=="-q" ( set _QUIET=true
) else if "%__ARG%"=="-quiet" ( set _QUIET=true
) else if "%__ARG%"=="-colors" ( set _COLORS=true
) else if "%__ARG%"=="-no-colors" ( set _COLORS=
) else if "%__ARG:~0,2%"=="-D" ( call :addJava "%__ARG%"
) else if "%__ARG:~0,2%"=="-J" ( call :addJava "%__ARG:~2%"
) else (
    if defined _IN_SCRIPTING_ARGS ( call :addScripting "%__ARG%"
    ) else ( call :addResidual "%__ARG%"
    )
)
shift
goto args_loop
:args_done
goto :eof

@rem output parameter: _SCALA_ARGS
:addScala
set _SCALA_ARGS=%_SCALA_ARGS% %~1
goto :eof

@rem output parameter: _JAVA_ARGS
:addJava
set _JAVA_ARGS=%_JAVA_ARGS% %~1
goto :eof

@rem output parameter: _RESIDUAL_ARGS
:addResidual
set _RESIDUAL_ARGS=%_RESIDUAL_ARGS% %~1
goto :eof

@rem output parameter: _CLASS_PATH_FILE.
@rem It contains the path th the absolute classpaths
:classpathArgs

set _ETC_DIR="%_PROG_HOME%\etc"
set _TMP_DIR="%_PROG_HOME%\tmp"
set _INPUT_CLASSPATH_FILE="%_ETC_DIR%\scaladoc.classpath"
set _OUTPUT_CLASSPATH_FILE="%_TMP_DIR%\scaladoc.classpath"

@rem Check if the _TMP_DIR exists. If not, create it (first time we run the command)
if not exist "%TMP_DIR%" ( mkdir "%TMP_DIR%" )

@rem If the file doesn't exist, create it (first time we run the command)
if not exist "%_OUTPUT_CLASSPATH_FILE%" ( call :loadClasspathFromFile )
set _CLASS_PATH_FILE="%_OUTPUT_CLASSPATH_FILE%"
goto :eof

@REM concatentate every line in _INPUT_CLASSPATH_FILE and dump it in the with _PSEP
:loadClasspathFromFile
set _CLASS_PATH=
if exist "%_INPUT_CLASSPATH_FILE%" (
    for /f "usebackq delims=" %%i in "%_INPUT_CLASSPATH_FILE%" do (
        set _LIB="%_PROG_HOME%\maven2\%%i"
        @rem Adapt the paths from Unix style to MS-DOS style
        set "_LIB=!_LIB:/=\!"
        @rem Append the processed line to the output file, ensuring a new line
        echo !_LIB! >> "%_OUTPUT_CLASSPATH_FILE%"
    )
)
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
exit /b %_EXITCODE%
endlocal
