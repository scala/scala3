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
set _ARG=%~1
if %_DEBUG%==1 echo [%_BASENAME%] _ARG=%_ARG%
if "%_ARG%"=="--" (
    rem for arg; do addResidual "$arg"; done; set -- ;;
) else if /i "%_ARG%"=="-h" (
    set _HELP=true
    call :addScala "-help"
) else if /i "%_ARG%"=="-help" (
    set _HELP=true
    call :addScala "-help"
) else if /i "%_ARG%"=="-v" (
    set _VERBOSE=true
    call :addScala "-verbose"
) else if /i "%_ARG%"=="-verbose" (
    set _VERBOSE=true
    call :addScala "-verbose"
) else if /i "%_ARG%"=="-debug" ( set _JAVA_DEBUG=%_DEBUG_STR%
) else if /i "%_ARG%"=="-q" ( set _QUIET=true
) else if /i "%_ARG%"=="-quiet" ( set _QUIET=true
rem Optimize for short-running applications, see https://github.com/lampepfl/dotty/issues/222
) else if "%_ARG%"=="-=short" (
    call :addJava "-XX:+TieredCompilation -XX:TieredStopAtLevel=1"
) else if /i "%_ARG%"=="-repl" ( set _PROG_NAME=%_REPL_MAIN%
) else if /i "%_ARG%"=="-compile" ( set _PROG_NAME=%_COMPILER_MAIN%
) else if /i "%_ARG%"=="-decompile" ( set _PROG_NAME=%_DECOMPILER_MAIN%
) else if /i "%_ARG%"=="print-tasty" (
    set _PROG_NAME=%_DECOMPILER_MAIN%
    call :addScala "-print-tasty"
) else if /i "%_ARG%"=="-run" ( set _PROG_NAME=%_REPL_MAIN%
) else if /i "%_ARG%"=="-colors" ( set _COLORS=true
) else if /i "%_ARG%"=="-no-colors" ( set _COLORS=
) else if /i "%_ARG%"=="-with-compiler" ( set _JVM_CP_ARGS=%_PSEP%%_DOTTY_COMP%
rem break out -D and -J options and add them to JAVA_OPTS as well
rem so they reach the JVM in time to do some good. The -D options
rem will be available as system properties.
) else if "%_ARG:~0,2%"=="-D" (
    call :addJava "%_ARG%"
    call :addScala "%_ARG%"
) else if "%_ARG:~0,2%"=="-J" (
    call :addJava "%_ARG%"
    call :addScala "%_ARG%"
) else (
    call :addResidual "%_ARG%"
)
shift
goto args_loop
:args_done
if %_DEBUG%==1 (
    echo [%_BASENAME%] _VERBOSE=%_VERBOSE%
    echo [%_BASENAME%] _PROG_NAME=%_PROG_NAME%
)
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

