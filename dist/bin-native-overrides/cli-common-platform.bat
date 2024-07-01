@echo off

setlocal enabledelayedexpansion

set "_SCALA_CLI_VERSION="
@rem read for cli_version:=_SCALA_CLI_VERSION in EXTRA_PROPERTIES file
FOR /F "usebackq delims=" %%G IN ("%_PROG_HOME%\EXTRA_PROPERTIES") DO (
  SET "line=%%G"
  IF "!line:~0,13!"=="cli_version:=" (
    SET "_SCALA_CLI_VERSION=!line:~13!"
    GOTO :foundCliVersion
  )
)

@REM we didn't find it, so we should fail
echo "ERROR: cli_version not found in EXTRA_PROPERTIES file"
exit /b 1

:foundCliVersion
endlocal & set "SCALA_CLI_VERSION=%_SCALA_CLI_VERSION%"

set SCALA_CLI_CMD_WIN="%_PROG_HOME%\bin\scala-cli.exe" "--cli-version" "%SCALA_CLI_VERSION%"
