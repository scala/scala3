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

set _COMPILER_MAIN=dotty.tools.dotc.Main
set _DECOMPILER_MAIN=dotty.tools.dotc.decompiler.Main
set _REPL_MAIN=dotty.tools.repl.Main

set _PROG_NAME=%_COMPILER_MAIN%

call :args %*

rem ##########################################################################
rem ## Main

call :classpathArgs

if defined JAVA_OPTS ( set _JAVA_OPTS=%JAVA_OPTS%
) else ( set _JAVA_OPTS=-Xmx768m -Xms768m
)
if %_DEBUG%==1 echo [%_BASENAME%] "%_JAVACMD%" %_JAVA_OPTS% %_JAVA_DEBUG% %_JAVA_ARGS% %_JVM_CP_ARGS% -Dscala.usejavacp=true %_PROG_NAME% %_SCALA_ARGS% %_RESIDUAL_ARGS%
"%_JAVACMD%" %_JAVA_OPTS% %_JAVA_DEBUG% %_JAVA_ARGS% %_JVM_CP_ARGS% ^
-Dscala.usejavacp=true ^
%_PROG_NAME% %_SCALA_ARGS% %_RESIDUAL_ARGS%
if not %ERRORLEVEL%==0 (
    if %_DEBUG%==1 echo [%_BASENAME%] Dotty compiler execution failed
    set _EXITCODE=1
    goto end
)
goto end

rem ##########################################################################
rem ## Subroutines

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
set __ARG=%~1
if %_DEBUG%==1 echo [%_BASENAME%] __ARG=%__ARG%
if "%__ARG%"=="--" (
    rem for arg; do addResidual "$arg"; done; set -- ;;
) else if /i "%__ARG%"=="-h" (
    set _HELP=true
    call :addScala "-help"
) else if /i "%__ARG%"=="-help" (
    set _HELP=true
    call :addScala "-help"
) else if /i "%__ARG%"=="-v" (
    set _VERBOSE=true
    call :addScala "-verbose"
) else if /i "%__ARG%"=="-verbose" (
    set _VERBOSE=true
    call :addScala "-verbose"
) else if /i "%__ARG%"=="-debug" ( set _JAVA_DEBUG=%_DEBUG_STR%
) else if /i "%__ARG%"=="-q" ( set _QUIET=true
) else if /i "%__ARG%"=="-quiet" ( set _QUIET=true
rem Optimize for short-running applications, see https://github.com/lampepfl/dotty/issues/222
) else if "%__ARG%"=="-=short" (
    call :addJava "-XX:+TieredCompilation -XX:TieredStopAtLevel=1"
) else if /i "%__ARG%"=="-repl" ( set _PROG_NAME=%_REPL_MAIN%
) else if /i "%__ARG%"=="-compile" ( set _PROG_NAME=%_COMPILER_MAIN%
) else if /i "%__ARG%"=="-decompile" ( set _PROG_NAME=%_DECOMPILER_MAIN%
) else if /i "%__ARG%"=="print-tasty" (
    set _PROG_NAME=%_DECOMPILER_MAIN%
    call :addScala "-print-tasty"
) else if /i "%__ARG%"=="-run" ( set _PROG_NAME=%_REPL_MAIN%
) else if /i "%__ARG%"=="-colors" ( set _COLORS=true
) else if /i "%__ARG%"=="-no-colors" ( set _COLORS=
) else if /i "%__ARG%"=="-with-compiler" ( set _JVM_CP_ARGS=%_PSEP%%_DOTTY_COMP%
rem break out -D and -J options and add them to JAVA_OPTS as well
rem so they reach the JVM in time to do some good. The -D options
rem will be available as system properties.
) else if "%_ARG:~0,2%"=="-D" (
    call :addJava "%__ARG%"
    call :addScala "%__ARG%"
) else if "%_ARG:~0,2%"=="-J" (
    call :addJava "%__ARG%"
    call :addScala "%__ARG%"
) else (
    call :addResidual "%__ARG%"
)
shift
goto args_loop
:args_done
if %_DEBUG%==1 echo [%_BASENAME%] _VERBOSE=%_VERBOSE%
if %_DEBUG%==1 echo [%_BASENAME%] _PROG_NAME=%_PROG_NAME%
goto :eof

rem output parameter: _SCALA_ARGS
:addScala
set _SCALA_ARGS=%_SCALA_ARGS% %~1
if %_DEBUG%==1 echo [%_BASENAME%] _SCALA_ARGS=%_SCALA_ARGS%
goto :eof

rem output parameter: _JAVA_ARGS
:addJava
set _JAVA_ARGS=%_JAVA_ARGS% %~1
if %_DEBUG%==1 echo [%_BASENAME%] _JAVA_ARGS=%_JAVA_ARGS%
goto :eof

rem output parameter: _RESIDUAL_ARGS
:addResidual
set _RESIDUAL_ARGS=%_RESIDUAL_ARGS% %~1
if %_DEBUG%==1 echo [%_BASENAME%] _RESIDUAL_ARGS=%_RESIDUAL_ARGS%
goto :eof

rem output parameter: _JVM_CP_ARGS
:classpathArgs
rem echo dotty-compiler: %_DOTTY_COMP%
rem echo dotty-interface: %_DOTTY_INTF%
rem echo dotty-library: %_DOTTY_LIB%
rem echo scala-asm: %_SCALA_ASM%
rem echo scala-lib: %_SCALA_LIB%
rem echo scala-xml: %_SCALA_XML%
rem echo sbt-intface: %_SBT_INTF%

set __TOOLCHAIN=%_SCALA_LIB%%_PSEP%
set __TOOLCHAIN=%__TOOLCHAIN%%_SCALA_ASM%%_PSEP%
set __TOOLCHAIN=%__TOOLCHAIN%%_SBT_INTF%%_PSEP%
set __TOOLCHAIN=%__TOOLCHAIN%%_DOTTY_INTF%%_PSEP%
set __TOOLCHAIN=%__TOOLCHAIN%%_DOTTY_LIB%%_PSEP%
set __TOOLCHAIN=%__TOOLCHAIN%%_DOTTY_COMP%%_PSEP%

rem # jline
set __TOOLCHAIN=%__TOOLCHAIN%%_JLINE_READER%%_PSEP%
set __TOOLCHAIN=%__TOOLCHAIN%%_JLINE_TERMINAL%%_PSEP%
set __TOOLCHAIN=%__TOOLCHAIN%%_JLINE_TERMINAL_JNA%%_PSEP%
set __TOOLCHAIN=%__TOOLCHAIN%%_JNA%

set _JVM_CP_ARGS=-classpath %__TOOLCHAIN%
goto :eof

rem ##########################################################################
rem ## Cleanups

:end
if %_DEBUG%==1 echo [%_BASENAME%] _EXITCODE=%_EXITCODE%
exit /b %_EXITCODE%
endlocal

