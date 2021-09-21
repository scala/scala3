@echo off
setlocal enabledelayedexpansion

@rem #########################################################################
@rem ## Environment setup

set _EXITCODE=0

set "_PROG_HOME=%~dp0"

call "%_PROG_HOME%\common.bat"
if not %_EXITCODE%==0 goto end

set _DEFAULT_JAVA_OPTS=-Xmx768m -Xms768m
@rem set _WITH_COMPILER=true

set _COMPILER_MAIN=dotty.tools.dotc.Main
set _DECOMPILER_MAIN=dotty.tools.dotc.decompiler.Main
set _REPL_MAIN=dotty.tools.repl.Main
set _SCRIPTING_MAIN=dotty.tools.scripting.Main

call :args %*

@rem #########################################################################
@rem ## Main

call :classpathArgs

set _SCRIPTING_STRING=
if "%_PROG_NAME%"=="%_SCRIPTING_MAIN%" (
    if not defined _TARGET_SCRIPT (
        echo Error: Missing Scala script file 1>&2
        set _EXITCODE=1
        goto end
    )
    set _SCRIPTING_STRING=-script %_TARGET_SCRIPT% %_SCRIPTING_ARGS%
)

if defined JAVA_OPTS ( set _JAVA_OPTS=%JAVA_OPTS%
) else ( set _JAVA_OPTS=%_DEFAULT_JAVA_OPTS%
)
call "%_JAVACMD%" %_JAVA_OPTS% %_JAVA_DEBUG% %_JAVA_ARGS% %_JVM_CP_ARGS% ^
-Dscala.usejavacp=true ^
%_PROG_NAME% %_SCALA_ARGS% %_RESIDUAL_ARGS% %_SCRIPTING_STRING%
if not %ERRORLEVEL%==0 (
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
set _PROG_NAME=%_COMPILER_MAIN%
set _SCALA_ARGS=
set _JAVA_ARGS=
set _RESIDUAL_ARGS=
set _SCRIPTING_ARGS=
set _TARGET_SCRIPT=

:args_loop
if "%~1"=="" goto args_done
set "__ARG=%~1"
if defined _TARGET_SCRIPT (
    call :addScripting "%__ARG%"
) else if "%__ARG%"=="--" (
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
@rem Optimize for short-running applications, see https://github.com/lampepfl/dotty/issues/222
) else if "%__ARG%"=="-Oshort" (
    call :addJava "-XX:+TieredCompilation -XX:TieredStopAtLevel=1"
) else if "%__ARG%"=="-repl" ( set _PROG_NAME=%_REPL_MAIN%
) else if "%__ARG%"=="-script" (
    set _PROG_NAME=%_SCRIPTING_MAIN%
    if "%~2"=="" goto args_done
    set "_TARGET_SCRIPT=%~2"
    shift
) else if "%__ARG%"=="-compile" ( set _PROG_NAME=%_COMPILER_MAIN%
) else if "%__ARG%"=="-decompile" ( set _PROG_NAME=%_DECOMPILER_MAIN%
) else if "%__ARG%"=="-print-tasty" (
    set _PROG_NAME=%_DECOMPILER_MAIN%
    call :addScala "-print-tasty"
) else if "%__ARG%"=="-run" ( set _PROG_NAME=%_REPL_MAIN%
) else if "%__ARG%"=="-colors" ( set _COLORS=true
) else if "%__ARG%"=="-no-colors" ( set _COLORS=
) else if "%__ARG%"=="-with-compiler" ( set "_JVM_CP_ARGS=%_PSEP%%_SCALA3_COMP%%_PSEP%%_TASTY_CORE%"
@rem break out -D and -J options and add them to JAVA_OPTS as well
@rem so they reach the JVM in time to do some good. The -D options
@rem will be available as system properties.
) else if "%__ARG:~0,2%"=="-D" ( call :addJava "%__ARG%"
) else if "%__ARG:~0,2%"=="-J" ( call :addJava "%__ARG:~2%"
) else ( call :addResidual "%__ARG%"
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

@rem output parameter: _SCRIPTING_ARGS
:addScripting
set _SCRIPTING_ARGS=%_SCRIPTING_ARGS% %~1
goto :eof

@rem output parameter: _JVM_CP_ARGS
:classpathArgs
@rem echo scala3-compiler: %_SCALA3_COMP%
@rem echo scala3-interface: %_SCALA3_INTF%
@rem echo scala3-library: %_SCALA3_LIB%
@rem echo tasty-core: %_TASTY_CORE%
@rem echo scala-asm: %_SCALA_ASM%
@rem echo scala-lib: %_SCALA_LIB%
@rem echo sbt-intface: %_SBT_INTF%

set "__TOOLCHAIN=%_SCALA_LIB%%_PSEP%"
set "__TOOLCHAIN=%__TOOLCHAIN%%_SCALA3_LIB%%_PSEP%"
set "__TOOLCHAIN=%__TOOLCHAIN%%_SCALA_ASM%%_PSEP%"
set "__TOOLCHAIN=%__TOOLCHAIN%%_SBT_INTF%%_PSEP%"
set "__TOOLCHAIN=%__TOOLCHAIN%%_SCALA3_INTF%%_PSEP%"
set "__TOOLCHAIN=%__TOOLCHAIN%%_SCALA3_COMP%%_PSEP%"
set "__TOOLCHAIN=%__TOOLCHAIN%%_TASTY_CORE%%_PSEP%"
set "__TOOLCHAIN=%__TOOLCHAIN%%_SCALA3_STAGING%%_PSEP%"
set "__TOOLCHAIN=%__TOOLCHAIN%%_SCALA3_TASTY_INSPECTOR%%_PSEP%"

@rem # jline
set "__TOOLCHAIN=%__TOOLCHAIN%%_JLINE_READER%%_PSEP%"
set "__TOOLCHAIN=%__TOOLCHAIN%%_JLINE_TERMINAL%%_PSEP%"
set "__TOOLCHAIN=%__TOOLCHAIN%%_JLINE_TERMINAL_JNA%%_PSEP%"
set "__TOOLCHAIN=%__TOOLCHAIN%%_JNA%

set _JVM_CP_ARGS=-classpath "%__TOOLCHAIN%"
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
exit /b %_EXITCODE%
endlocal
