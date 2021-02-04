@echo off
setlocal enabledelayedexpansion

@rem only for interactive debugging
set _DEBUG=0

@rem #########################################################################
@rem ## Environment setup

set _EXITCODE=0

call :env
if not %_EXITCODE%==0 goto end

call :args %*
if not %_EXITCODE%==0 goto end

@rem #########################################################################
@rem ## Main

if %_HELP%==1 (
    call :help
    exit /b !_EXITCODE!
)
if %_RUN%==1 (
    call :run
    if not !_EXITCODE!==0 goto end
)
goto end

@rem #########################################################################
@rem ## Subroutines

:env
set _BASENAME=%~n0
set "_ROOT_DIR=%~dp0"

set _DEBUG_LABEL=[DEBUG]
set _ERROR_LABEL=Error:

set "_DOWNLOADS_DIR=%_ROOT_DIR%downloads"
set "_TEXMF_LOCAL_DIR=%_ROOT_DIR%texmf-local"

@rem We extract tar.xz archives with commands tar and xz
if not exist "%GIT_HOME%\usr\bin\tar.exe" (
    echo %_ERROR_LABEL% tar executable not found 1>&2
    set _EXITCODE=1
    goto :eof
)
set "_TAR_CMD=%GIT_HOME%\usr\bin\tar.exe"
set "_CYGPATH_CMD=%GIT_HOME%\usr\bin\cygpath.exe"

if not exist "%GIT_HOME%\mingw64\bin\xz.exe" (
    echo %_ERROR_LABEL% xz executable not found 1>&2
    set _EXITCODE=1
    goto :eof
)
set "_XZ_CMD=%GIT_HOME%\mingw64\bin\xz.exe"

if not exist "%TEXLIVE_HOME%\bin\win32\texhash.exe" (
    echo %_ERROR_LABEL% texhash executable not found ^(check variable TEXLIVE_HOME^) 1>&2
    set _EXITCODE=1
    goto :eof
)
set "_TEXHASH_CMD=%TEXLIVE_HOME%\bin\win32\texhash.exe"
goto :eof

@rem input parameter: %*
@rem output parameters: _HELP, _RUN, _VERBOSE
:args
set _HELP=0
set _RUN=0
set _VERBOSE=0
set __N=0
:args_loop
set "__ARG=%~1"
if not defined __ARG (
    if !__N!==0 set _HELP=1
    goto args_done
)
if "%__ARG:~0,1%"=="-" (
    @rem option
    if "%__ARG%"=="-debug" ( set _DEBUG=1
    ) else if "%__ARG%"=="-help" ( set _HELP=1
    ) else if "%__ARG%"=="-verbose" ( set _VERBOSE=1
    ) else (
        echo %_ERROR_LABEL% Unknown option %__ARG% 1>&2
        set _EXITCODE=1
        goto args_done
    )
) else (
    @rem subcommand
    if "%__ARG%"=="help" ( set _HELP=1
    ) else if "%__ARG%"=="run" ( set _RUN=1
    ) else (
        echo %_ERROR_LABEL% Unknown subcommand %__ARG% 1>&2
        set _EXITCODE=1
        goto args_done
    )
    set /a __N+=1
)
shift
goto :args_loop
:args_done
set _REDIRECT_STDOUT=1^>NUL
if %_DEBUG%==1 set _REDIRECT_STDOUT=1^>^&2

if %_DEBUG%==1 (
    echo %_DEBUG_LABEL% Options    : _HELP=%_HELP% _VERBOSE=%_VERBOSE% 1>&2
    echo %_DEBUG_LABEL% Subcommands: _RUN=%_RUN% 1>&2
    echo %_DEBUG_LABEL% Variables  : TEXLIVE_HOME=%TEXLIVE_HOME% 1>&2
)
goto :eof

:help
echo Usage: %_BASENAME% { ^<option^> ^| ^<subcommand^> }
echo.
echo   Options:
echo     -debug      display commands executed by this script
echo     -verbose    display environment settings
echo.
echo   Subcommands:
echo     help        display this help message
echo     run         download and install additional TeX fonts
goto :eof

:run
if not exist "%_DOWNLOADS_DIR%" mkdir "%_DOWNLOADS_DIR%"

call :download
if not !_EXITCODE!==0 goto :eof

call :install
if not !_EXITCODE!==0 goto :eof

goto :eof

:install
for /f %%f in ('dir /b "%_DOWNLOADS_DIR%\*.tar.xz"') do (
    set "__XZ_FILE=%_DOWNLOADS_DIR%\%%f"
    set "__TAR_FILE=!__XZ_FILE:~0,-3!"
    if not exist "!__TAR_FILE!" (
        if %_DEBUG%==1 echo %_DEBUG_LABEL% "%_XZ_CMD%" -dk "!__XZ_FILE!" 1>&2
        call "%_XZ_CMD%" -dk "!__XZ_FILE!"
    )
    for /f %%i in ('%_CYGPATH_CMD% "%_TEXMF_LOCAL_DIR%"') do set "__C_ARG=%%i"
    for /f %%i in ('%_CYGPATH_CMD% "!__TAR_FILE!"') do set "__XF_ARG=%%i"
    if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_TAR_CMD%" -C "!__C_ARG!" -xf "!__XF_ARG!" 1>&2
    ) else if %_VERBOSE%==1 ( echo Extract fonts from archive file "!__TAR_FILE:%TEMP%=%%TEMP%%!" 1>&2
    )
    call "%_TAR_CMD%" -C "!__C_ARG!" -xf "!__XF_ARG!"
    @rem del "!__TAR_FILE!"
)
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_TEXHASH_CMD%" 1>&2
) else if %_VERBOSE%==1 echo Rebuild database indices 1>&2
)
call "%_TEXHASH_CMD%" %_REDIRECT_STDOUT%
if not %ERRORLEVEL%==0 (
    set _EXITCODE=1
    goto :eof
)
goto :eof

:download
@rem Update path part '2021/01/20' to retrieve newer archive files
set __ARCHIVE_URL=https://texlive.info/tlnet-archive/2021/01/20/tlnet/archive
set __ARCHIVE_FILES=environ fancyvrb fontspec gnu-freefont
set __ARCHIVE_FILES=%__ARCHIVE_FILES% l3backend l3kernel l3packages pgf
set __ARCHIVE_FILES=%__ARCHIVE_FILES% tcolorbox trimspaces unicode-math xcolor

for %%i in (%__ARCHIVE_FILES%) do (
    set "__XZ_NAME=%%i.tar.xz"
    set "__XZ_FILE=%_DOWNLOADS_DIR%\!__XZ_NAME!"
    if not exist "!__XZ_FILE!" (
        set "__XZ_URL=%__ARCHIVE_URL%/!__XZ_NAME!"
        if %_DEBUG%==1 ( echo %_DEBUG_LABEL% powershell -c "Invoke-WebRequest -Uri '!__XZ_URL!' -Outfile '!__XZ_FILE!'" 1>&2
        ) else if %_VERBOSE%==1 ( echo Download archive file !__XZ_NAME! to directory "!_DOWNLOADS_DIR:%TEMP%=%%TEMP%%!" 1>&2
        )
        powershell -c "$progressPreference='silentlyContinue';Invoke-WebRequest -Uri '!__XZ_URL!' -Outfile '!__XZ_FILE!'"
        if not !ERRORLEVEL!==0 (
            echo %_ERROR_LABEL% Failed to download file %__XZ_NAME% 1>&2
            set _EXITCODE=1
            goto :eof
        )
    )
)
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
if %_DEBUG%==1 echo %_DEBUG_LABEL% _EXITCODE=%_EXITCODE% 1>&2
exit /b %_EXITCODE%
endlocal
