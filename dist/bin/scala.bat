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

@rem #########################################################################
@rem ## Main

call :setScalaOpts

@rem SCALA_CLI_CMD_WIN is an array, set in cli-common-platform.bat
call %SCALA_CLI_CMD_WIN% "--prog-name" "scala" "--cli-default-scala-version" "%_SCALA_VERSION%" "-r" "%MVN_REPOSITORY%" %*

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

@REM set _PROG_HOME to the substring from the first colon to the end
set "_PROG_HOME_SUB=!_PROG_HOME:~%index%!"
@REM strip initial character
set "_PROG_HOME_SUB=!_PROG_HOME_SUB:~1!"

@REM set drive to substring from 0 to the first colon
set "_PROG_HOME_DRIVE=!_PROG_HOME:~0,%index%!"



set "_SCALA_VERSION="
set "MVN_REPOSITORY=file://%_PROG_HOME_DRIVE%\%_PROG_HOME_SUB:\=/%/maven2"

call "%_PROG_HOME%\bin\cli-common-platform.bat"

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
