@rem ## Created by mics (https://github.com/michelou/) on December 2020.

@echo off
setlocal enabledelayedexpansion

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
if %_CLEAN%==1 (
    call :clean
    if not !_EXITCODE!==0 goto end
)
if %_COMPILE%==1 (
    call :compile
    if not !_EXITCODE!==0 goto end
)
if %_RUN%==1 (
    call :run
    if not !_EXITCODE!==0 goto end
)
goto end

@rem #########################################################################
@rem ## Subroutines

@rem output parameters: _DEBUG_LABEL, _ERROR_LABEL, _WARNING_LABEL
@rem                    _SOURCE_DIR, _TARGET_DIR, _PANDOC_CMD
:env
set _TIMER=0
set _BASENAME=%~n0
set "_ROOT_DIR=%~dp0"

call :env_colors
set _DEBUG_LABEL=%_NORMAL_BG_CYAN%[%_BASENAME%]%_RESET%
set _ERROR_LABEL=%_STRONG_FG_RED%Error%_RESET%:
set _WARNING_LABEL=%_STRONG_FG_YELLOW%Warning%_RESET%:

set "_DATA_DIR=%_ROOT_DIR%data"
set "_IMAGES_DIR=%_DATA_DIR%\images"
set "_TEMPLATES_DIR=%_DATA_DIR%\templates"
for %%f in ("%~dp0\.") do set "_TARGET_DIR=%%~dpfout\pandoc"
goto :eof

:env_colors
@rem ANSI colors in standard Windows 10 shell
@rem see https://gist.github.com/mlocati/#file-win10colors-cmd
set _RESET=[0m
set _BOLD=[1m
set _UNDERSCORE=[4m
set _INVERSE=[7m

@rem normal foreground colors
set _NORMAL_FG_BLACK=[30m
set _NORMAL_FG_RED=[31m
set _NORMAL_FG_GREEN=[32m
set _NORMAL_FG_YELLOW=[33m
set _NORMAL_FG_BLUE=[34m
set _NORMAL_FG_MAGENTA=[35m
set _NORMAL_FG_CYAN=[36m
set _NORMAL_FG_WHITE=[37m

@rem normal background colors
set _NORMAL_BG_BLACK=[40m
set _NORMAL_BG_RED=[41m
set _NORMAL_BG_GREEN=[42m
set _NORMAL_BG_YELLOW=[43m
set _NORMAL_BG_BLUE=[44m
set _NORMAL_BG_MAGENTA=[45m
set _NORMAL_BG_CYAN=[46m
set _NORMAL_BG_WHITE=[47m

@rem strong foreground colors
set _STRONG_FG_BLACK=[90m
set _STRONG_FG_RED=[91m
set _STRONG_FG_GREEN=[92m
set _STRONG_FG_YELLOW=[93m
set _STRONG_FG_BLUE=[94m
set _STRONG_FG_MAGENTA=[95m
set _STRONG_FG_CYAN=[96m
set _STRONG_FG_WHITE=[97m

@rem strong background colors
set _STRONG_BG_BLACK=[100m
set _STRONG_BG_RED=[101m
set _STRONG_BG_GREEN=[102m
set _STRONG_BG_YELLOW=[103m
set _STRONG_BG_BLUE=[104m
goto :eof

@rem input parameter: %*
:args
set _CLEAN=0
set _COMPILE=0
set _HELP=0
set _PROJECT_NAME=reference
set _RUN=0
set _TIMER=0
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
    ) else if "%__ARG:~0,9%"=="-project:" (
        call :set_project "!__ARG:~9!"
        if not !_EXITCODE!== 0 goto args_done
    ) else if "%__ARG%"=="-timer" ( set _TIMER=1
    ) else if "%__ARG%"=="-verbose" ( set _VERBOSE=1
    ) else (
        echo %_ERROR_LABEL% Unknown option %__ARG% 1>&2
        set _EXITCODE=1
        goto args_done
    )
) else (
    @rem subcommand
    if "%__ARG%"=="clean" ( set _CLEAN=1
    ) else if "%__ARG%"=="compile" ( set _COMPILE=1
    ) else if "%__ARG%"=="help" ( set _HELP=1
    ) else if "%__ARG%"=="run" ( set _COMPILE=1& set _RUN=1
    ) else if "%__ARG%"=="view" ( set _COMPILE=1& set _RUN=1
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
set "_OUTPUT_FILE=%_TARGET_DIR%\scala3_%_PROJECT_NAME%.pdf"

if %_DEBUG%==1 (
    echo %_DEBUG_LABEL% Options    : _TIMER=%_TIMER% _VERBOSE=%_VERBOSE% 1>&2
    echo %_DEBUG_LABEL% Subcommands: _CLEAN=%_CLEAN% _COMPILE=%_COMPILE% _RUN=%_RUN% 1>&2
    echo %_DEBUG_LABEL% Variables  : PANDOC_HOME=%PANDOC_HOME% 1>&2
	echo %_DEBUG_LABEL% Variables  : TEXLIVE_HOME=%TEXLIVE_HOME% 1>&2
    echo %_DEBUG_LABEL% Variables  : _PROJECT_NAME=%_PROJECT_NAME% 1>&2
)
if %_TIMER%==1 for /f "delims=" %%i in ('powershell -c "(Get-Date)"') do set _TIMER_START=%%i
goto :eof

:help
if %_VERBOSE%==1 (
    set __BEG_P=%_STRONG_FG_CYAN%
    set __BEG_O=%_STRONG_FG_GREEN%
    set __BEG_N=%_NORMAL_FG_YELLOW%
    set __END=%_RESET%
) else (
    set __BEG_P=
    set __BEG_O=
    set __BEG_N=
    set __END=
)
echo Usage: %__BEG_O%%_BASENAME% { ^<option^> ^| ^<subcommand^> }%__END%
echo.
echo   %__BEG_P%Options:%__END%
echo     %__BEG_O%-debug%__END%           show commands executed by this script
echo     %__BEG_O%-help%__END%            display this help message
echo     %__BEG_O%-project:^<name^>%__END%  project name ^(default: %__BEG_O%reference%__END%^)
echo     %__BEG_O%-verbose%__END%         display progress messages
echo.
echo   %__BEG_P%Subcommands:%__END%
echo     %__BEG_O%clean%__END%            delete generated files
echo     %__BEG_O%compile%__END%          compile source files
echo     %__BEG_O%run%__END%              open generated PDF file in default application
echo     %__BEG_O%view%__END%             alias for %__BEG_O%run%__END%
goto :eof

@rem output parameter: _PROJECT_NAME
:set_project
set __ARG=%~1

if "%__ARG%"=="internals" ( set _PROJECT_NAME=internals
) else if "%__ARG%"=="reference" ( set _PROJECT_NAME=reference
) else if "%__ARG%"=="usage" ( set _PROJECT_NAME=usage
) else (
    echo %_ERROR_LABEL% Invalid project name ^(%__ARG%^) 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

:clean
call :rmdir "%_TARGET_DIR%"
goto :eof

@rem input parameter(s): %1=directory path
:rmdir
set __DIR=%~1
if not exist "%__DIR%\" goto :eof
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% rmdir /s /q "%__DIR%" 1>&2
) else if %_VERBOSE%==1 ( echo Delete directory "!__DIR:%_ROOT_DIR%=!" 1>&2
)
rmdir /s /q "%__DIR%"
if not %ERRORLEVEL%==0 (
    set _EXITCODE=1
    goto :eof
)
goto :eof

:compile
if not exist "%_TARGET_DIR%" mkdir "%_TARGET_DIR%"

if not exist "%_OUTPUT_FILE%" goto :md2pdf

call :compile_required "%_OUTPUT_FILE%" "%_IMAGES_DIR%\*.png"
if %_COMPILE_REQUIRED%==1 goto :md2pdf

call :compile_required "%_OUTPUT_FILE%" "%_TEMPLATES_DIR%\*.tex"
if %_COMPILE_REQUIRED%==1 goto :md2pdf

call :compile_required "%_OUTPUT_FILE%" "%_SOURCE_DIR%\%_PROJECT_NAME%\*.md"
if %_COMPILE_REQUIRED%==0 goto :eof

:md2pdf
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_ROOT_DIR%md2pdf.bat" "%_PROJECT_NAME%" 1>&2
) else if %_VERBOSE%==1 ( echo md2pdf.bat "%_PROJECT_NAME%" 1>&2
)
call "%_ROOT_DIR%md2pdf.bat" "%_PROJECT_NAME%"
if not %ERRORLEVEL%==0 (
    set _EXITCODE=1
    goto :eof
)
goto :eof

@rem input parameter: 1=target file 2,3,..=path (wildcards accepted)
@rem output parameter: _COMPILE_REQUIRED
:compile_required
set "__TARGET_FILE=%~1"

set __PATH_ARRAY=
set __PATH_ARRAY1=
:compile_path
shift
set __PATH=%~1
if not defined __PATH goto :compile_next
set __PATH_ARRAY=%__PATH_ARRAY%,'%__PATH%'
set __PATH_ARRAY1=%__PATH_ARRAY1%,'!__PATH:%_ROOT_DIR%=!'
goto :compile_path

:compile_next
set __TARGET_TIMESTAMP=00000000000000
for /f "usebackq" %%i in (`powershell -c "gci -path '%__TARGET_FILE%' -ea Stop | select -last 1 -expandProperty LastWriteTime | Get-Date -uformat %%Y%%m%%d%%H%%M%%S" 2^>NUL`) do (
     set __TARGET_TIMESTAMP=%%i
)
set __SOURCE_TIMESTAMP=00000000000000
for /f "usebackq" %%i in (`powershell -c "gci -recurse -path %__PATH_ARRAY:~1% -ea Stop | sort LastWriteTime | select -last 1 -expandProperty LastWriteTime | Get-Date -uformat %%Y%%m%%d%%H%%M%%S" 2^>NUL`) do (
    set __SOURCE_TIMESTAMP=%%i
)
call :newer %__SOURCE_TIMESTAMP% %__TARGET_TIMESTAMP%
set _COMPILE_REQUIRED=%_NEWER%
if %_DEBUG%==1 (
    echo %_DEBUG_LABEL% %__TARGET_TIMESTAMP% Target : '%__TARGET_FILE%' 1>&2
    echo %_DEBUG_LABEL% %__SOURCE_TIMESTAMP% Sources: %__PATH_ARRAY:~1% 1>&2
    echo %_DEBUG_LABEL% _COMPILE_REQUIRED=%_COMPILE_REQUIRED% 1>&2
) else if %_VERBOSE%==1 if %_COMPILE_REQUIRED%==0 if %__SOURCE_TIMESTAMP% gtr 0 (
    echo No compilation needed ^(%__PATH_ARRAY1:~1%^) 1>&2
)
goto :eof

@rem output parameter: _NEWER
:newer
set __TIMESTAMP1=%~1
set __TIMESTAMP2=%~2

set __DATE1=%__TIMESTAMP1:~0,8%
set __TIME1=%__TIMESTAMP1:~-6%

set __DATE2=%__TIMESTAMP2:~0,8%
set __TIME2=%__TIMESTAMP2:~-6%

if %__DATE1% gtr %__DATE2% ( set _NEWER=1
) else if %__DATE1% lss %__DATE2% ( set _NEWER=0
) else if %__TIME1% gtr %__TIME2% ( set _NEWER=1
) else ( set _NEWER=0
)
goto :eof

:run
if not exist "%_OUTPUT_FILE%" (
    echo %_ERROR_LABEL% Output file not found ^("%_OUTPUT_FILE%"^) 1>&2
    set _EXITCODE=1
    goto :eof
)
if %_DEBUG%==1 echo %_DEBUG_LABEL% start "%_PROJECT_NAME%" "%_OUTPUT_FILE%" 1>&2
start "%_PROJECT_NAME%" "%_OUTPUT_FILE%"
goto :eof

@rem output parameter: _DURATION
:duration
set __START=%~1
set __END=%~2

for /f "delims=" %%i in ('powershell -c "$interval = New-TimeSpan -Start '%__START%' -End '%__END%'; Write-Host $interval"') do set _DURATION=%%i
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
if %_TIMER%==1 (
    for /f "delims=" %%i in ('powershell -c "(Get-Date)"') do set __TIMER_END=%%i
    call :duration "%_TIMER_START%" "!__TIMER_END!"
    echo Total elapsed time: !_DURATION! 1>&2
)
if %_DEBUG%==1 echo %_DEBUG_LABEL% _EXITCODE=%_EXITCODE% 1>&2
exit /b %_EXITCODE%
endlocal
