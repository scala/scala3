@echo off

rem ##########################################################################
rem ## This batch file is based on shell script project/scripts/genDocs

setlocal enabledelayedexpansion

rem only for interactive debugging
set _DEBUG=0

rem ##########################################################################
rem ## Environment setup

set _BASENAME=%~n0

set _EXITCODE=0

set _BOT_TOKEN=dotty-token

for %%f in ("%~dp0..\..") do set _ROOT_DIR=%%~sf
set _SCRIPTS_DIR=%_ROOT_DIR%\project\scripts

if not defined __COMMON__ (
    if %_DEBUG%==1 echo [%_BASENAME%] call %_SCRIPTS_DIR%\common.bat
    call %_SCRIPTS_DIR%\common.bat
    if not !_EXITCODE!==0 goto end
)

rem ##########################################################################
rem ## Main

rem # make sure that _BOT_TOKEN is set
if not defined _BOT_TOKEN (
    echo Error: _BOT_TOKEN env unset, unable to push without password 1>&2
    set _EXITCODE=1
    goto end
)
for /f %%i in ('cd') do set _PWD=%%~si

echo Working directory: %_PWD%

call "%_SBT_CMD%" genDocs

rem # make sure that the previous command actually succeeded
if not exist "%_PWD%\docs\_site\" (
    echo Error: output directory did not exist: %_PWD%\docs\_site 1>&2
    set _EXITCODE=1
    goto end
)

rem # save current head for commit message in gh-pages
rem for /f %%i in ('%_GIT_CMD% rev-parse HEAD 2^>NUL') do set _GIT_HEAD=%%i

rem # set up remote and github credentials
rem %_GIT_CMD% remote add doc-remote "https://dotty-bot:%_BOT_TOKEN%@github.com/lampepfl/dotty-website.git"
rem %_GIT_CMD% config user.name "dotty-bot"
rem %_GIT_CMD% config user.email "dotty-bot@d-d.me"

rem # check out correct branch
rem %_GIT_CMD% fetch doc-remote gh-pages
rem %_GIT_CMD% checkout gh-pages

rem # move newly generated _site dir to $PWD
rem move %_PWD%\docs\_site .

rem # remove everything BUT _site dir
rem del /f /q /s -rf !(_site)

rem # copy new contents to $PWD
rem move _site\* .

rem # remove now empty _site dir
rem del /f /q /s _site

rem # add all contents of $PWD to commit
rem %_GIT_CMD% add -A
rem %_GIT_CMD% commit -m "Update gh-pages site for %_GIT_HEAD%" || echo "nothing new to commit"

rem # push to doc-remote
rem %_GIT_CMD% push doc-remote || echo "couldn't push, since nothing was added"

goto end

rem ##########################################################################
rem ## Subroutines

rem ##########################################################################
rem ## Cleanups

:end
if %_DEBUG%==1 echo [%_BASENAME%] _EXITCODE=%_EXITCODE%
exit /b %_EXITCODE%
endlocal
