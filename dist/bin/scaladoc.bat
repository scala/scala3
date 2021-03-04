@echo off
setlocal enabledelayedexpansion

@rem #########################################################################
@rem ## Environment setup

set _EXITCODE=0

set "_PROG_HOME=%~dp0"

call "%_PROG_HOME%\common.bat"
if not %_EXITCODE%==0 goto end

set _DEFAULT_JAVA_OPTS=-Xmx768m -Xms768m

@rem set _COMPILER_MAIN=dotty.tools.dotc.Main
@rem set _DECOMPILER_MAIN=dotty.tools.dotc.decompiler.Main
set _DOC_MAIN=dotty.tools.scaladoc.Main
@rem set _REPL_MAIN=dotty.tools.repl.Main
@rem set _SCRIPTING_MAIN=dotty.tools.scripting.Main

set _PROG_NAME=%_DOC_MAIN%

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
%_PROG_NAME% %_SCALA_ARGS% %_RESIDUAL_ARGS%
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
for /f %%f in ("%_PROG_HOME%\.") do set "__LIB_DIR=%%~dpflib"

@rem Set scaladoc dep:
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*scaladoc*"') do set _SCALA3_DOC=%__LIB_DIR%\%%f

@rem Set flexmark deps:
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-0*"')                     do set _FLEXMARK_LIBS=%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-ext-anchorlink*"')        do set _FLEXMARK_LIBS=%_FLEXMARK_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-ext-autolink*"')          do set _FLEXMARK_LIBS=%_FLEXMARK_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-ext-emoji*"')             do set _FLEXMARK_LIBS=%_FLEXMARK_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-ext-gfm-strikethrough*"') do set _FLEXMARK_LIBS=%_FLEXMARK_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-ext-gfm-tables*"')        do set _FLEXMARK_LIBS=%_FLEXMARK_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-ext-gfm-tasklist*"')      do set _FLEXMARK_LIBS=%_FLEXMARK_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-ext-ins*"')               do set _FLEXMARK_LIBS=%_FLEXMARK_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-ext-superscript*"')       do set _FLEXMARK_LIBS=%_FLEXMARK_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-ext-tables*"')            do set _FLEXMARK_LIBS=%_FLEXMARK_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-ext-wikilink*"')          do set _FLEXMARK_LIBS=%_FLEXMARK_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-ext-yaml-front-matter*"') do set _FLEXMARK_LIBS=%_FLEXMARK_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-formatter*"')             do set _FLEXMARK_LIBS=%_FLEXMARK_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-html-parser*"')           do set _FLEXMARK_LIBS=%_FLEXMARK_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-jira-converter*"')        do set _FLEXMARK_LIBS=%_FLEXMARK_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*flexmark-util*"')                  do set _FLEXMARK_LIBS=%_FLEXMARK_LIBS%%__LIB_DIR%\%%f%_PSEP%

@rem Set jackson deps:
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*jackson-annotations*"')     do set _JACKSON_LIBS=%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*jackson-core*"')            do set _JACKSON_LIBS=%_JACKSON_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*jackson-databind*"')        do set _JACKSON_LIBS=%_JACKSON_LIBS%%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*jackson-dataformat-yaml*"') do set _JACKSON_LIBS=%_JACKSON_LIBS%%__LIB_DIR%\%%f%_PSEP%

@rem Set liqp dep:
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*liqp*"') do set _LIQP_LIB=%__LIB_DIR%\%%f%_PSEP%

@rem Set ANTLR dep:
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*antlr-3*"') do set _ANTLR_LIB=%__LIB_DIR%\%%f%_PSEP%
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*antlr-runtime-3*"') do set _ANTLR_RUNTIME_LIB=%__LIB_DIR%\%%f%_PSEP%

@rem Set autolink dep:
@rem conflict with flexmark-ext-autolink-0.11
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*autolink-0.6*"') do set _AUTOLINK_LIB=%__LIB_DIR%\%%f%_PSEP%

@rem Set Protobuf dep:
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*protobuf-java*"') do set _PROTOBUF_LIB=%__LIB_DIR%\%%f%_PSEP%

@rem Set snakeyaml dep:
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*snakeyaml*"') do set _SNAKEYAML_LIB=%__LIB_DIR%\%%f%_PSEP%

@rem Set ST4 dep:
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*ST4*"') do set _ST4_LIB=%__LIB_DIR%\%%f%_PSEP%

@rem Set jsoup dep:
for /f %%f in ('dir /a-d /b "%__LIB_DIR%\*jsoup*"') do set _JSOUP_LIB=%__LIB_DIR%\%%f%_PSEP%

set _CLASS_PATH=%_SCALA3_DOC%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_SCALA3_LIB%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_SCALA3_COMP%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_TASTY_CORE%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_SCALA3_INTF%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_SBT_INTF%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_SCALA3_STAGING%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_SCALA3_TASTY_INSPECTOR%
@rem set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_SCALA_ASM%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_SCALA_LIB%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_FLEXMARK_LIBS%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_JACKSON_LIBS%
@rem set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_JLINE_READER%
@rem set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_JLINE_TERMINAL%
@rem set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_JLINE_TERMINAL_JNA%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_LIQP_LIB%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_ANTLR_LIB%%_PSEP%%_ANTLR_RUNTIME_LIB%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_AUTOLINK_LIB%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_PROTOBUF_LIB%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_SNAKEYAML_LIB%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_ST4_LIB%
set _CLASS_PATH=%_CLASS_PATH%%_PSEP%%_JSOUP_LIB%
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
exit /b %_EXITCODE%
endlocal
