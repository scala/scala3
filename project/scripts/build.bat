@echo off

rem ##########################################################################
rem ## This batch file is based on configuration file .drone.yml

setlocal enabledelayedexpansion

rem only for interactive debugging
set _DEBUG=0

rem ##########################################################################
rem ## Environment setup

set _BASENAME=%~n0

set _EXITCODE=0

for %%f in ("%~dp0..\..") do set _ROOT_DIR=%%~sf
set _SCRIPTS_DIR=%_ROOT_DIR%\project\scripts

call %_SCRIPTS_DIR%\common.bat
if not %_EXITCODE%==0 goto end

rem set _DRONE_BUILD_EVENT=pull_request
set _DRONE_BUILD_EVENT=
set _DRONE_REMOTE_URL=
set _DRONE_BRANCH=

call :args %*
if not %_EXITCODE%==0 goto end
if defined _HELP call :help & exit /b %_EXITCODE%

rem ##########################################################################
rem ## Main

call :init
if not %_EXITCODE%==0 goto end

if defined _CLEAN_ALL (
    call :clean_all
    if not !_EXITCODE!==0 goto end
)
if defined _CLONE (
    call :clone
    if not !_EXITCODE!==0 goto end
)
if defined _COMPILE (
    call :test
    if not !_EXITCODE!==0 goto end
)
if defined _BOOTSTRAP (
    call :test_bootstrapped
    rem if not !_EXITCODE!==0 goto end
    if not !_EXITCODE!==0 (
        if defined _IGNORE ( echo ###### Warning: _EXITCODE=!_EXITCODE! ####### 1>&2
        ) else ( goto end
        )
    )
)
if defined _SBT (
    call :test_sbt
    if not !_EXITCODE!==0 goto end
)
if defined _DOCUMENTATION (
    call :documentation
    if not !_EXITCODE!==0 goto end
)
if defined _ARCHIVES (
    call :archives
    if not !_EXITCODE!==0 goto end
)
goto end

rem ##########################################################################
rem ## Subroutines

rem input parameter: %*
rem output parameters: _CLONE, _COMPILE, _DOCUMENTATION, _SBT, _TIMER, _VERBOSE
:args
set _ARCHIVES=
set _BOOTSTRAP=
set _COMPILE=
set _CLEAN_ALL=
set _CLONE=
set _DOCUMENTATION=
set _HELP=
set _SBT=
set _TIMER=0
set _VERBOSE=0

:args_loop
set __ARG=%~1
if not defined __ARG goto args_done
if /i "%__ARG%"=="help" ( set _HELP=1& goto :eof
) else if /i "%__ARG%"=="-timer" ( set _TIMER=1
) else if /i "%__ARG%"=="-verbose" ( set _VERBOSE=1
) else if /i "%__ARG:~0,4%"=="arch" (
    if not "%__ARG:~-5%"=="-only" set _CLONE=1& set _COMPILE=1& set _BOOTSTRAP=1
    set _ARCHIVES=1
) else if /i "%__ARG:~0,4%"=="boot" (
    if not "%__ARG:~-5%"=="-only" set _CLONE=1& set _COMPILE=1
    set _BOOTSTRAP=1
) else if /i "%__ARG%"=="cleanall" ( set _CLEAN_ALL=1
) else if /i "%__ARG%"=="clone" ( set _CLONE=1
) else if /i "%__ARG:~0,7%"=="compile" (
    if not "%__ARG:~-5%"=="-only" set _CLONE=1
    set _COMPILE=1
) else if /i "%__ARG:~0,3%"=="doc" (
    if not "%__ARG:~-5%"=="-only" set _CLONE=1& set _COMPILE=1& set _BOOTSTRAP=1
    set _DOCUMENTATION=1
) else if /i "%__ARG%"=="sbt" (
    set _CLONE=1& set _COMPILE=1& set _BOOTSTRAP=1& set _SBT=1
) else if /i "%__ARG%"=="sbt-only" (
    set _SBT=1
) else (
    echo Error: Unknown subcommand %__ARG% 1>&2
    set _EXITCODE=1
    goto :eof
)
shift
goto args_loop
:args_done
if %_TIMER%==1 (
    for /f "delims=" %%i in ('powershell -c "(Get-Date)"') do set _TIMER_START=%%i
)
goto :eof

:help
echo Usage: %_BASENAME% { options ^| subcommands }
echo   Options:
echo     -timer                 display total execution time
echo     -verbose               display environment settings
echo   Subcommands:
echo     arch[ives]             generate gz/zip archives (after bootstrap)
echo     boot[strap]            generate+test bootstrapped compiler (after compile)
echo     cleanall               clean project (sbt+git) and quit
echo     clone                  update submodules
echo     compile                generate+test 1st stage compiler (after clone)
echo     doc[umentation]        generate documentation (after bootstrap)
echo     help                   display this help message
echo     sbt                    test sbt-dotty (after bootstrap)
echo   Advanced subcommands (no deps):
echo     arch[ives]-only        generate ONLY gz/zip archives
echo     boot[strap]-only       generate+test ONLY bootstrapped compiler
echo     compile-only           generate+test ONLY 1st stage compiler
echo     doc[umentation]-only]  generate ONLY documentation
echo     sbt-only               test ONLY sbt-dotty

goto :eof

:init
if %_VERBOSE%==1 (
    for /f %%i in ('where git.exe') do set __GIT_CMD1=%%i
    set __GIT_BRANCH=unknown
    for /f "tokens=1-4,*" %%f in ('!__GIT_CMD1! branch -vv ^| findstr /b *') do set __GIT_BRANCH=%%g %%i
    echo Tool paths
    echo    GIT_CMD=!__GIT_CMD1!
    echo    SBT_CMD=%_SBT_CMD%
    echo Tool options
    echo    JAVA_OPTS=%JAVA_OPTS%
    echo    SBT_OPTS=%SBT_OPTS%
    echo Current Git branch
    echo    !__GIT_BRANCH!
    echo.
)
goto :eof

:clean_all
echo run sbt clean and git clean -xdf --exclude=*.bat --exclude=*.ps1
if %_DEBUG%==1 echo [%_BASENAME%] call "%_SBT_CMD%" clean
call "%_SBT_CMD%" clean
if not %ERRORLEVEL%==0 (
    set _EXITCODE=1
    goto :eof
)
if %_DEBUG%==1 echo [%_BASENAME%] %_GIT_CMD% clean -xdf --exclude=*.bat --exclude=*.ps1
%_GIT_CMD% clean -xdf --exclude=*.bat --exclude=*.ps1
if not %ERRORLEVEL%==0 (
    set _EXITCODE=1
    goto :eof
)
goto :eof

:clone
if "%_DRONE_BUILD_EVENT%"=="pull_request" if defined _DRONE_REMOTE_URL (
    %_GIT_CMD% config user.email "dotty.bot@epfl.ch"
    %_GIT_CMD% config user.name "Dotty CI"
    %_GIT_CMD% pull "%_DRONE_REMOTE_URL%" "%_DRONE_BRANCH%"
)
if %_DEBUG%==1 echo [%_BASENAME%] %_GIT_CMD% submodule update --init --recursive --jobs 3
%_GIT_CMD% submodule update --init --recursive --jobs 3
if not %ERRORLEVEL%==0 (
    echo Error: Failed to update Git submodules 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

:test
echo sbt compile and sbt test
if %_DEBUG%==1 echo [%_BASENAME%] call "%_SBT_CMD%" ";compile ;test"
call "%_SBT_CMD%" ";compile ;test"
if not %ERRORLEVEL%==0 (
    echo Error: Failed to build Dotty 1>&2
    set _EXITCODE=1
    goto :eof
)

rem see shell script project/scripts/cmdTests
if %_DEBUG%==1 echo [%_BASENAME%] call %_SCRIPTS_DIR%\cmdTests.bat
call %_SCRIPTS_DIR%\cmdTests.bat
if not %ERRORLEVEL%==0 (
    echo Error: Failed to run cmdTest.bat 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

:test_bootstrapped
if %_DEBUG%==1 echo [%_BASENAME%] call "%_SBT_CMD%" ";dotty-bootstrapped/compile ;dotty-bootstrapped/test"
call "%_SBT_CMD%" ";dotty-bootstrapped/compile ;dotty-bootstrapped/test"
if not %ERRORLEVEL%==0 (
    echo Error: Failed to bootstrap Dotty 1>&2
    set _EXITCODE=1
    goto :eof
)

rem see shell script project/scripts/bootstrapCmdTests
if %_DEBUG%==1 echo [%_BASENAME%] call %_SCRIPTS_DIR%\bootstrapCmdTests.bat
call %_SCRIPTS_DIR%\bootstrapCmdTests.bat
if not %ERRORLEVEL%==0 (
    echo Error: Failed to run bootstrapCmdTests.bat 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

:test_sbt
if %_DEBUG%==1 echo [%_BASENAME%] call "%_SBT_CMD%" sbt-dotty/scripted
call "%_SBT_CMD%" sbt-dotty/scripted
if not %ERRORLEVEL%==0 (
    echo Error: Failed to test sbt-dotty 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

:documentation
rem see shell script project/scripts/genDocs
if %_DEBUG%==1 echo [%_BASENAME%] call %_SCRIPTS_DIR%\genDocs.bat
call %_SCRIPTS_DIR%\genDocs.bat
if not %ERRORLEVEL%==0 (
    set _EXITCODE=1
    goto :eof
)
goto :eof

:archives
if %_DEBUG%==1 echo [%_BASENAME%] call "%_SBT_CMD%" dist-bootstrapped/packArchive
call "%_SBT_CMD%" dist-bootstrapped/packArchive
rem output directory for gz/zip archives
set __TARGET_DIR=%_ROOT_DIR%\dist-bootstrapped\target
if not exist "%__TARGET_DIR%\" (
    echo Error: Directory target not found 1>&2
    set _EXITCODE=1
    goto :eof
)
if %_DEBUG%==1 (
    echo.
    echo Output directory: %__TARGET_DIR%\
    dir /b /a-d "%__TARGET_DIR%"
)
goto :eof

rem output parameter: _DURATION
:duration
set __START=%~1
set __END=%~2

for /f "delims=" %%i in ('powershell -c "$interval = New-TimeSpan -Start '%__START%' -End '%__END%'; Write-Host $interval"') do set _DURATION=%%i
goto :eof

rem input parameter: 1=start time
:total
set __TIMER_START=%~1

for /f "delims=" %%i in ('powershell -c "(Get-Date)"') do set __TIMER_END=%%i
call :duration "%_TIMER_START%" "!__TIMER_END!"
echo Total execution time: %_DURATION%
goto :eof

rem ##########################################################################
rem ## Cleanups

:end
if %_TIMER%==1 call :total "%_TIMER_START%"
if %_DEBUG%==1 echo [%_BASENAME%] _EXITCODE=%_EXITCODE%
exit /b %_EXITCODE%
endlocal
