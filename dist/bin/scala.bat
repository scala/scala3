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

@rem we need to escape % in the java command path, for some reason this doesnt work in common.bat
set "_JAVACMD=!_JAVACMD:%%=%%%%!"

call "%_JAVACMD%" "-jar" "%SCALA_CLI_JAR%" "--cli-default-scala-version" "%_SCALA_VERSION%" "-r" "%MVN_REPOSITORY%" %*
if not %ERRORLEVEL%==0 ( set _EXITCODE=1& goto end )

goto end

@rem #########################################################################
@rem ## Subroutines

:setScalaOpts

set "_SCALA_VERSION="
set "MVN_REPOSITORY=file://%_PROG_HOME:\=/%/maven2"
set "SCALA_CLI_JAR=%_PROG_HOME%\etc\scala-cli.jar"

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
