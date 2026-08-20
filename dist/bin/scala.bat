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
call "%_PROG_HOME%\libexec\common.bat"
@rem Reset exit code: common.bat may fail if Java is not installed, but the native
@rem Scala CLI launcher (scala-cli.exe) does not require Java. A missing Java error
@rem will surface later only if the JVM-based launcher is used instead.
set _EXITCODE=0

@rem #########################################################################
@rem ## Main

call :setScalaOpts

if not defined _SCALA_VERSION (
    echo Error: Failed to extract Scala version from "%_PROG_HOME%\VERSION" 1>&2
    set _EXITCODE=1
    goto end
)

call "%_PROG_HOME%\libexec\cli-common-platform.bat"

@rem SCALA_CLI_CMD_WIN is set by cli-common-platform.bat.
@rem We use delayed expansion (!var!) and avoid 'call' to prevent double-expansion
@rem of percent signs, which would corrupt %20 in the Maven repository URI.
!SCALA_CLI_CMD_WIN! "--prog-name" "scala" "--skip-cli-updates" "--cli-default-scala-version" "!_SCALA_VERSION!" "-r" "!MVN_REPOSITORY!" %*

if not %ERRORLEVEL%==0 ( set _EXITCODE=1& goto end )

goto end

@rem #########################################################################
@rem ## Subroutines

:setScalaOpts

set "_SCALA_VERSION="

@rem Read the VERSION file to extract the Scala version.
@rem We use FOR /F with a 'type' command and delayed expansion (!_PROG_HOME!) so
@rem that parentheses in the path (e.g. "C:\Program Files (x86)\scala") are not
@rem present at parse time when cmd.exe matches the IN (...) / DO (...) blocks.
@rem Unlike 'set /p', FOR /F correctly splits lines on both LF and CRLF.
if not exist "%_PROG_HOME%\VERSION" goto :setMvnRepo
for /f "delims=" %%G in ('type "!_PROG_HOME!\VERSION"') do (
    set "_LINE=%%G"
    if "!_LINE:~0,9!"=="version:=" (
        set "_SCALA_VERSION=!_LINE:~9!"
        goto :setMvnRepo
    )
)

:setMvnRepo
@rem Construct the local Maven repository URI.
@rem Backslashes are converted to forward slashes; spaces are percent-encoded.
set "_PROG_HOME_FWD=!_PROG_HOME:\=/!"
set "MVN_REPOSITORY=file:///!_PROG_HOME_FWD: =%%20!/maven2"

goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
exit /b %_EXITCODE%
endlocal
