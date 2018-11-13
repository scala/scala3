@echo off
setlocal enabledelayedexpansion

rem only for interactive debugging
set _DEBUG=0

rem ##########################################################################
rem ## Environment setup

set _BASENAME=%~n0

set _EXITCODE=0

set _BOT_TOKEN=dotty-token

rem set _DRONE_BUILD_EVENT=pull_request
set _DRONE_BUILD_EVENT=
set _DRONE_REMOTE_URL=
set _DRONE_BRANCH=

for %%f in ("%~dp0..\..") do set _ROOT_DIR=%%~sf
set _BIN_DIR=%_ROOT_DIR%bin
set _TESTS_POS_DIR=%_ROOT_DIR%test\pos

set _SOURCE=tests/pos/HelloWorld.scala
set _MAIN=HelloWorld
set _EXPECTED_OUTPUT=hello world

call :args %*
if not %_EXITCODE%==0 goto end
if defined _HELP call :help & exit /b %_EXITCODE%

if exist "C:\Temp\" ( set _TMP_DIR=C:\Temp
) else ( set _TMP_DIR=%TEMP%
)
set _OUT_DIR=%_TMP_DIR%\%_BASENAME%_out
if not exist "%_OUT_DIR%" mkdir "%_OUT_DIR%"

set _OUT1_DIR=%_TMP_DIR%\%_BASENAME%_out1
if not exist "%_OUT1_DIR%" mkdir "%_OUT1_DIR%"

set _TMP_FILE=%_TMP_DIR%\%_BASENAME%_tmp.txt

where /q git.exe
if not %ERRORLEVEL%==0 (
    echo Error: Git command not found ^(check your PATH variable^) 1>&2
    set _EXITCODE=1
    goto end
)
set _GIT_CMD=git.exe

where /q sbt.bat
if not %ERRORLEVEL%==0 (
    echo Error: SBT command not found ^(check your PATH variable^) 1>&2
    set _EXITCODE=1
    goto end
)
rem full path is required for sbt to run successfully
for /f %%i in ('where sbt.bat') do set _SBT_CMD=%%i

rem see file project/scripts/sbt
rem SBT uses the value of the JAVA_OPTS environment variable if defined, rather than the config.
set JAVA_OPTS=-Xmx4096m ^
-XX:ReservedCodeCacheSize=1024m ^
-XX:MaxMetaspaceSize=1024m

set SBT_OPTS=-Ddotty.drone.mem=4096m ^
-Dsbt.ivy.home=%USERPROFILE%\.ivy2\ ^
-Dsbt.log.noformat=true

rem ##########################################################################
rem ## Main

if %_VERBOSE%==1 (
    for /f %%i in ('where git.exe') do set _GIT_CMD1=%%i
    echo _GIT_CMD=!_GIT_CMD1!
    echo _SBT_CMD=%_SBT_CMD%
    echo JAVA_OPTS=%JAVA_OPTS%
    echo SBT_OPTS=%SBT_OPTS%
    echo.
)
if defined _CLEAN_ALL (
    call :clean_all
    if not !_EXITCODE!==0 goto end
)
if defined _CLONE (
    call :clone
    if not !_EXITCODE!==0 goto end
)
if defined _BUILD (
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
rem output parameters: _VERBOSE, _DOCUMENTATION
:args
set _ARCHIVES=
set _BOOTSTRAP=
set _BUILD=
set _CLEAN_ALL=
set _DOCUMENTATION=
set _HELP=
set _VERBOSE=0

:args_loop
set __ARG=%~1
if not defined __ARG goto args_done
if /i "%__ARG%"=="help" ( set _HELP=1& goto :eof
) else if /i "%__ARG%"=="-verbose" ( set _VERBOSE=1
) else if /i "%__ARG:~0,4%"=="arch" (
    if not "%__ARG:~-5%"=="-only" set _BUILD=1& set _BOOTSTRAP=1
    set _ARCHIVES=1
) else if /i "%__ARG:~0,4%"=="boot" (
    if not "%__ARG:~-5%"=="-only" set _BUILD=1
    set _BOOTSTRAP=1
) else if /i "%__ARG%"=="cleanall" ( set _CLEAN_ALL=1
) else if /i "%__ARG:~0,3%"=="doc" (
    if not "%__ARG:~-5%"=="-only" set _BUILD=1& set _BOOTSTRAP=1
    set _DOCUMENTATION=1
) else (
    echo %_BASENAME%: Unknown subcommand %__ARG%
    set _EXITCODE=1
    goto :eof
)
shift
goto args_loop
:args_done
goto :eof

:help
echo Usage: %_BASENAME% { options ^| subcommands }
echo   Options:
echo     -verbose               display environment settings
echo   Subcommands:
echo     arch[ives]             generate gz/zip archives (after bootstrap)
echo     arch[ives]-only        generate ONLY gz/zip archives
echo     boot[strap]            generate compiler bootstrap (after build)
echo     boot[strap]-only       generate ONLY compiler bootstrap
echo     cleanall               clean project (sbt+git) and quit
echo     doc[umentation]        generate documentation (after bootstrap)
echo     doc[umentation]-only]  generate ONLY documentation
echo     help                   display this help message
goto :eof

:clean_all
if %_DEBUG%==1 echo [%_BASENAME%] call "%_SBT_CMD%" clean
call "%_SBT_CMD%" clean
if not %ERRORLEVEL%==0 (
    set _EXITCODE=1
    goto :eof
)
if %_DEBUG%==1 echo [%_BASENAME%] %_GIT_CMD% clean -xdf
%_GIT_CMD% clean -xdf
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

:clear_out
set __OUT_DIR=%~1

if exist "%__OUT_DIR%" (
    if %_DEBUG%==1 echo [%_BASENAME%] del /s /q "%__OUT_DIR%\*" 1^>NUL
    del /s /q "%__OUT_DIR%\*" 1>NUL
)
goto :eof

:grep 
set __PATTERN=%~1
set __FILE=%~2

if %_DEBUG%==1 echo [%_BASENAME%] findstr "%__PATTERN%" "%__FILE%
findstr "%__PATTERN%" "%__FILE%"
if not %ERRORLEVEL%==0 (
    echo Error: Failed to find pattern "%__PATTERN%" in file %__FILE% 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

rem ## see file project/scripts/cmdTests
:cmdTests
echo testing sbt dotc and dotr
if %_DEBUG%==1 echo [%_BASENAME%] "%_SBT_CMD%" ";dotc %_SOURCE% -d %_OUT_DIR% ;dotr -classpath %_OUT_DIR% %_MAIN%" ^> "%_TMP_FILE%"
call "%_SBT_CMD%" ";dotc %_SOURCE% -d %_OUT_DIR% ;dotr -classpath %_OUT_DIR% %_MAIN%" > "%_TMP_FILE%"
call :grep "%_EXPECTED_OUTPUT%" "%_TMP_FILE%"
if not %_EXITCODE%==0 goto :eof

rem # check that `sbt dotc` compiles and `sbt dotr` runs it
echo testing sbt dotc -from-tasty and dotr -classpath
call :clear_out "%_OUT_DIR%"
call "%_SBT_CMD%" ";dotc %_SOURCE% -d %_OUT_DIR% ;dotc -from-tasty -classpath %_OUT_DIR% -d %_OUT1_DIR% %_MAIN% ;dotr -classpath %_OUT1_DIR% %_MAIN%" > "%_TMP_FILE%"
call :grep "%_EXPECTED_OUTPUT%" "%_TMP_FILE%"
if not %_EXITCODE%==0 goto :eof

rem # check that `sbt dotc -decompile` runs
echo testing sbt dotc -decompile
call "%_SBT_CMD%" ";dotc -decompile -color:never -classpath %_OUT_DIR% %_MAIN%" > "%_TMP_FILE%"
call :grep "def main(args: scala.Array\[scala.Predef.String\]): scala.Unit =" "%_TMP_FILE%"
if not %_EXITCODE%==0 goto :eof

echo testing sbt dotr with no -classpath
call :clear_out "%_OUT_DIR%"
if %_DEBUG%==1 echo [%_BASENAME%] "%_SBT_CMD%" ";dotc %_SOURCE% ; dotr %_MAIN%" ^> "%_TMP_FILE%"
call "%_SBT_CMD%" ";dotc %_SOURCE% ; dotr %_MAIN%" > "%_TMP_FILE%"
call :grep "%_EXPECTED_OUTPUT%" "%_TMP_FILE%"
if not %_EXITCODE%==0 goto :eof

echo testing loading tasty from .tasty file in jar
call :clear_out "%_OUT_DIR%"
call "%_SBT_CMD%" ";dotc -d %_OUT_DIR%\out.jar %_SOURCE%; dotc -decompile -classpath %_OUT_DIR%/out.jar -color:never %_MAIN%" > "%_TMP_FILE%"
call :grep "def main(args: scala.Array\[scala.Predef.String\]): scala.Unit =" "%_TMP_FILE%"
if not %_EXITCODE%==0 goto :eof

goto :eof

:test
if %_DEBUG%==1 echo [%_BASENAME%] call "%_SBT_CMD%" ";compile ;test"
call "%_SBT_CMD%" ";compile ;test"
if not %ERRORLEVEL%==0 (
    echo Error: Failed to build Dotty 1>&2
    set _EXITCODE=1
    goto :eof
)

rem ## see shell script project/scripts/cmdTests
call :cmdTests
if not %_EXITCODE%==0 goto :eof

goto :eof

:test_pattern
set __PATTERN=%~1
set __FILE=%~2

set /p __PATTERN2=<"%__FILE%"
if not "%__PATTERN2%"=="%__PATTERN%" (
    echo Error: failed to find pattern "%__PATTERN%" in file %__FILE% 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

rem ## see shell script project/scripts/bootstrapCmdTests
:bootstrapCmdTests
rem # check that benchmarks can run
if %_DEBUG%==1 echo [%_BASENAME%] "%_SBT_CMD%" "dotty-bench/jmh:run 1 1 tests/pos/alias.scala"
call "%_SBT_CMD%" "dotty-bench/jmh:run 1 1 tests/pos/alias.scala"

rem # The above is here as it relies on the bootstrapped library.
call "%_SBT_CMD%" "dotty-bench-bootstrapped/jmh:run 1 1 tests/pos/alias.scala"
call "%_SBT_CMD%" "dotty-bench-bootstrapped/jmh:run 1 1 -with-compiler compiler/src/dotty/tools/dotc/core/Types.scala"

echo testing scala.quoted.Expr.run from sbt dotr
call "%_SBT_CMD%" ";dotty-compiler-bootstrapped/dotc tests/run-with-compiler/quote-run.scala; dotty-compiler-bootstrapped/dotr -with-compiler Test" > "%_TMP_FILE%"
call :grep "val a: scala.Int = 3" "%_TMP_FILE%"
if not %_EXITCODE%==0 goto :eof

rem # setup for `dotc`/`dotr` script tests
if %_DEBUG%==1 echo [%_BASENAME%] "%_SBT_CMD%" dist-bootstrapped/pack
call "%_SBT_CMD%" dist-bootstrapped/pack

rem # check that `dotc` compiles and `dotr` runs it
echo testing ./bin/dotc and ./bin/dotr
call :clear_out "%_OUT_DIR%"
call %_BIN_DIR%\dotc.bat "%_SOURCE%" -d "%_OUT_DIR%"
call %_BIN_DIR%\dotr.bat -classpath "%_OUT_DIR%" "%_MAIN%" > "%_TMP_FILE%"
call :test_pattern "%_EXPECTED_OUTPUT%" "%_TMP_FILE%"

rem # check that `dotc -from-tasty` compiles and `dotr` runs it
echo testing ./bin/dotc -from-tasty and dotr -classpath
call :clear_out "%_OUT1_DIR%"
call %_BIN_DIR%\dotc.bat -from-tasty -classpath "%_OUT_DIR%" -d "%_OUT1_DIR%" "%_MAIN%"
call %_BIN_DIR%\dotr.bat -classpath "%_OUT1_DIR%" "%_MAIN%" > "%_TMP_FILE%"
call :test_pattern "%_EXPECTED_OUTPUT%" "%_TMP_FILE%"

rem # echo ":quit" | ./dist-bootstrapped/target/pack/bin/dotr  # not supported by CI

echo testing ./bin/dotd
call :clear_out "%_OUT_DIR%"
call %_BIN_DIR%\dotd.bat -project Hello -siteroot "%_OUT_DIR%" "%_SOURCE%"

goto :eof

:test_bootstrapped
if %_DEBUG%==1 echo [%_BASENAME%] call "%_SBT_CMD%" ";dotty-bootstrapped/compile ;dotty-bootstrapped/test"
call "%_SBT_CMD%" ";dotty-bootstrapped/compile ;dotty-bootstrapped/test"
if not %ERRORLEVEL%==0 (
    echo Error: failed to bootstrap Dotty 1>&2
    set _EXITCODE=1
    goto :eof
)

call :bootstrapCmdTests
if not %_EXITCODE%==0 goto :eof

goto :eof

:documentation
rem # make sure that _BOT_TOKEN is set
if not defined _BOT_TOKEN (
    echo Error: _BOT_TOKEN env unset, unable to push without password 1>&2
    set _EXITCODE=1
    goto :eof
)
for /f %%i in ('cd') do set _PWD=%%~si

echo Working directory: %_PWD%

call "%_SBT_CMD%" genDocs

rem # make sure that the previous command actually succeeded
if not exist "%_PWD%\docs\_site\" (
    echo Error: output directory did not exist: %_PWD%\docs\_site 1>&2
    set _EXITCODE=1
    goto :eof
)

goto :eof

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

rem ##########################################################################
rem ## Cleanups

:end
if %_DEBUG%==1 echo [%_BASENAME%] _EXITCODE=%_EXITCODE%
exit /b %_EXITCODE%
