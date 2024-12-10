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

@rem #########################################################################
@rem ## Call the new PowerShell script with arguments

set "args=%*"
call powershell.exe -ExecutionPolicy Bypass -File "%_PROG_HOME%\bin\scaladoc.ps1" %args%
if %ERRORLEVEL% neq 0 (
    set _EXITCODE=1
)

exit /b %_EXITCODE%
endlocal
