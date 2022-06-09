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

call :args %*

@rem #########################################################################
@rem ## Main

call :compilerJavaClasspathArgs

call "%_JAVACMD%" %_JAVA_ARGS% -classpath "%_JVM_CP_ARGS%" "-Dscala.usejavacp=true" "-Dscala.home=%_PROG_HOME%" dotty.tools.MainGenericCompiler %_SCALA_ARGS%
if not %ERRORLEVEL%==0 (
    set _EXITCODE=1
    goto end
)
goto end

@rem #########################################################################
@rem ## Subroutines

:args
set _JAVA_ARGS=
set _SCALA_ARGS=
set _SCALA_CPATH=
@rem replace inner while loop used in bash script
set _CONSUME_REMAINING=

:args_loop
if "%~1"=="" goto args_done
set "__ARG=%~1"
if defined _CONSUME_REMAINING (
    set _SCALA_ARGS=!_SCALA_ARGS! "%__ARG%"
    shift
) else if "%__ARG%"=="--" (
    @rem pass all remaining arguments to scala, e.g. to avoid interpreting them here as -D or -J
    set _CONSUME_REMAINING=1
    set _SCALA_ARGS=!_SCALA_ARGS! "%__ARG%"
    shift
) else if "%__ARG%"=="-script" (
    @rem pass all remaining arguments to scala, e.g. to avoid interpreting them here as -D or -J
    set _CONSUME_REMAINING=1
    set _SCALA_ARGS=!_SCALA_ARGS! "%__ARG%"
    shift
) else if "%__ARG%"=="-Oshort" (
    @rem optimize for short-running applications, see https://github.com/lampepfl/dotty/issues/222
    set _JAVA_ARGS=!_JAVA_ARGS! "-XX:+TieredCompilation" "-XX:TieredStopAtLevel=1"
    set _SCALA_ARGS=!_SCALA_ARGS! -Oshort
    shift
) else if "%__ARG:~0,2%"=="-D" (
    @rem pass to scala as well: otherwise we lose it sometimes when we
    @rem need it, e.g. communicating with a server compiler.
    set _JAVA_ARGS=!_JAVA_ARGS! "%__ARG%"
    set _SCALA_ARGS=!_SCALA_ARGS! "%__ARG%"
) else if "%__ARG:~0,2%"=="-J" (
    @rem as with -D, pass to scala even though it will almost
    @rem never be used.
    set _JAVA_ARGS=!_JAVA_ARGS! %__ARG:~2%
    set _SCALA_ARGS=!_SCALA_ARGS! "%__ARG%"
) else if "%__ARG%"=="-classpath" (
    set "_SCALA_CPATH=%~2"
    shift
) else if "%__ARG%"=="-cp" (
    set "_SCALA_CPATH=%~2"
    shift
) else (
    set _SCALA_ARGS=!_SCALA_ARGS! "%__ARG%"
)
shift
goto args_loop
:args_done
goto :eof

@rem output parameter: _JVM_CP_ARGS
:compilerJavaClasspathArgs
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
set "__TOOLCHAIN=%__TOOLCHAIN%%_JNA%%_PSEP%"

if defined _SCALA_CPATH (
    set "_JVM_CP_ARGS=%__TOOLCHAIN%%_SCALA_CPATH%"
) else (
    set "_JVM_CP_ARGS=%__TOOLCHAIN%"
)
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
exit /b %_EXITCODE%
endlocal
