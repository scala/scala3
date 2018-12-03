@echo off
setlocal enabledelayedexpansion

rem # Wrapper for the published dotc/dotr script that check for file changes
rem # and use sbt to re build the compiler as needed.

rem ##########################################################################
rem ## Environment setup

set _BASENAME=%~n0

set _EXITCODE=0

for %%f in ("%~dp0..") do set _ROOT_DIR=%%~sf

rem # Marker file used to obtain the date of latest call to sbt-back
set _VERSION=%_ROOT_DIR%\dist-bootstrapped\target\pack\VERSION

rem ##########################################################################
rem ## Main

rem # Create the target if absent or if file changed in ROOT/compiler
call :new_files "%_VERSION%"

if exist "%_VERSION%" if %_NEW_FILES%==0 goto target
echo Building Dotty...
pushd %_ROOT%
sbt.bat "dist-bootstrapped/pack"
popd

:target
call %*

goto end

rem ##########################################################################
rem ## Subroutines

rem input parameter: %1=version file
rem Output parameter: _NEW_FILES
:new_files
set __VERSION_FILE=%~1

call :timestamp "%__VERSION_FILE%"
set __VERSION_TIMESTAMP=%_TIMESTAMP%

set __JAVA_SOURCE_FILES=
for /f %%i in ('dir /s /b "%_ROOT_DIR%compiler\*.java" 2^>NUL') do (
    set __JAVA_SOURCE_FILES=!__JAVA_SOURCE_FILES! %%i
)
set __SCALA_SOURCE_FILES=
for /f %%i in ('dir /s /b "%_ROOT_DIR%compiler\*.scala" 2^>NUL') do (
    set __SCALA_SOURCE_FILES=!__SCALA_SOURCE_FILES! %%i
)

call :compile_required "%__VERSION_TIMESTAMP%" "%__JAVA_SOURCE_FILES% %__SCALA_SOURCE_FILES%"
set _NEW_FILES=%_COMPILE_REQUIRED%

goto :eof

rem input parameter: 1=timestamp file 2=source files
rem output parameter: _COMPILE_REQUIRED
:compile_required
set __TIMESTAMP_FILE=%~1
set __SOURCE_FILES=%~2

set __SOURCE_TIMESTAMP=00000000000000
for %%i in (%__SOURCE_FILES%) do (
    call :timestamp "%%i"
    call :newer !_TIMESTAMP! !__SOURCE_TIMESTAMP!
    if !_NEWER!==1 set __SOURCE_TIMESTAMP=!_TIMESTAMP!
)
if exist "%__TIMESTAMP_FILE%" ( set /p __CLASS_TIMESTAMP=<%__TIMESTAMP_FILE%
) else ( set __CLASS_TIMESTAMP=00000000000000
)

call :newer %__SOURCE_TIMESTAMP% %__CLASS_TIMESTAMP%
set _COMPILE_REQUIRED=%_NEWER%
goto :eof

rem output parameter: _NEWER
:newer
set __TIMESTAMP1=%~1
set __TIMESTAMP2=%~2

set __TIMESTAMP1_DATE=%__TIMESTAMP1:~0,8%
set __TIMESTAMP1_TIME=%__TIMESTAMP1:~-6%

set __TIMESTAMP2_DATE=%__TIMESTAMP2:~0,8%
set __TIMESTAMP2_TIME=%__TIMESTAMP2:~-6%

if %__TIMESTAMP1_DATE% gtr %__TIMESTAMP2_DATE% ( set _NEWER=1
) else if %__TIMESTAMP1_DATE% lss %__TIMESTAMP2_DATE% ( set _NEWER=0
) else if %__TIMESTAMP1_TIME% gtr %__TIMESTAMP2_TIME% ( set _NEWER=1
) else ( set _NEWER=0
)
goto :eof

rem input parameter: 1=file path
rem output parameter: _TIMESTAMP
:timestamp
set __FILE_PATH=%~1

set _TIMESTAMP=00000000000000
for /f %%i in ('powershell -C "(Get-ChildItem '%__FILE_PATH%').LastWriteTime | Get-Date -uformat %%Y%%m%%d%%H%%M%%S"') do (
    set _TIMESTAMP=%%i
)
goto :eof

rem ##########################################################################
rem ## Cleanups

:end
exit /b %_EXITCODE%
endlocal