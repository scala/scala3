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
if not %_EXITCODE%==0 goto end

@rem #########################################################################
@rem ## Main

call :setScalaOpts

call "%_PROG_HOME%\libexec\cli-common-platform.bat"

@rem SCALA_CLI_CMD_WIN is an array, set in cli-common-platform.bat.
@rem WE NEED TO PASS '--skip-cli-updates' for JVM launchers but we actually don't need it for native launchers
call %SCALA_CLI_CMD_WIN% "--prog-name" "scala" "--skip-cli-updates" "--cli-default-scala-version" "%_SCALA_VERSION%" "-r" "%MVN_REPOSITORY%" %*

if not %ERRORLEVEL%==0 ( set _EXITCODE=1& goto end )

goto end

@rem #########################################################################
@rem ## Subroutines

:setScalaOpts

@REM sfind the index of the first colon in _PROG_HOME
set "index=0"
set "char=!_PROG_HOME:~%index%,1!"
:findColon
if not "%char%"==":" (
  set /a "index+=1"
  set "char=!_PROG_HOME:~%index%,1!"
  goto :findColon
)

set "_SCALA_VERSION="
set "MVN_REPOSITORY=file:///%_PROG_HOME:\=/%/maven2"

@rem read for version:=_SCALA_VERSION in VERSION_FILE
FOR /F "usebackq delims=" %%G IN ("%_PROG_HOME%\VERSION") DO (
  SET "line=%%G"
  IF "!line:~0,9!"=="version:=" (
    SET "_SCALA_VERSION=!line:~9!"
    GOTO :foundVersion
  )
)

:foundVersion
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
exit /b %_EXITCODE%
endlocal
