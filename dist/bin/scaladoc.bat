@echo off
setlocal enabledelayedexpansion

@rem #########################################################################
@rem ## Environment setup

set _EXITCODE=0

set "_PROG_HOME=%~dp0"

call "%_PROG_HOME%\common.bat"
if not %_EXITCODE%==0 goto end

set _DEFAULT_JAVA_OPTS=-Xmx768m -Xms768m

call :args %*

@rem #########################################################################
@rem ## Main

call :classpathArgs

if defined JAVA_OPTS ( set _JAVA_OPTS=%JAVA_OPTS%
) else ( set _JAVA_OPTS=%_DEFAULT_JAVA_OPTS%
)
call "%_JAVACMD%" %_JAVA_OPTS% %_JAVA_DEBUG% %_JAVA_ARGS% ^
-classpath "%_CLASS_PATH%" ^
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

@rem output parameter: _CLASS_PATH
:classpathArgs
for /f "delims=" %%f in ("%_PROG_HOME%\.") do set "_LIB_DIR=%%~dpflib"
set _CLASS_PATH=
@rem keep list in sync with bash script `bin\scaladoc` !
call :updateClasspath "scaladoc"
call :updateClasspath "scala3-compiler"
call :updateClasspath "scala3-interfaces"
call :updateClasspath "scala3-library"
call :updateClasspath "tasty-core"
call :updateClasspath "scala3-staging"
call :updateClasspath "scala3-tasty-inspector"
call :updateClasspath "flexmark-0"
call :updateClasspath "flexmark-html-parser"
call :updateClasspath "flexmark-ext-anchorlink"
call :updateClasspath "flexmark-ext-autolink"
call :updateClasspath "flexmark-ext-emoji"
call :updateClasspath "flexmark-ext-gfm-strikethrough"
call :updateClasspath "flexmark-ext-gfm-tables"
call :updateClasspath "flexmark-ext-gfm-tasklist"
call :updateClasspath "flexmark-ext-wikilink"
call :updateClasspath "flexmark-ext-yaml-front-matter"
call :updateClasspath "liqp"
call :updateClasspath "jsoup"
call :updateClasspath "jackson-dataformat-yaml"
call :updateClasspath "jackson-datatype-jsr310"
call :updateClasspath "strftime4j"
call :updateClasspath "scala-asm"
call :updateClasspath "compiler-interface"
call :updateClasspath "jline-reader"
call :updateClasspath "jline-terminal-3"
call :updateClasspath "jline-terminal-jna"
call :updateClasspath "flexmark-util"
call :updateClasspath "flexmark-formatter"
call :updateClasspath "autolink-0.6"
call :updateClasspath "flexmark-jira-converter"
call :updateClasspath "antlr4"
call :updateClasspath "jackson-annotations"
call :updateClasspath "jackson-core"
call :updateClasspath "jackson-databind"
call :updateClasspath "snakeyaml"
call :updateClasspath "scala-library"
call :updateClasspath "protobuf-java"
call :updateClasspath "util-interface"
call :updateClasspath "jna-5"
call :updateClasspath "flexmark-ext-tables"
call :updateClasspath "flexmark-ext-ins"
call :updateClasspath "flexmark-ext-superscript"
call :updateClasspath "antlr4-runtime"
goto :eof

@rem input parameter: %1=pattern for library file
@rem output parameter: _CLASS_PATH
:updateClasspath
set "__PATTERN=%~1"
for /f "delims=" %%f in ('dir /a-d /b "%_LIB_DIR%\*%__PATTERN%*" 2^>NUL') do (
    set "_CLASS_PATH=!_CLASS_PATH!%_LIB_DIR%\%%f%_PSEP%"
)
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
exit /b %_EXITCODE%
endlocal
