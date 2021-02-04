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
if %_BUILD%==1 (
    call :build
    if not !_EXITCODE!==0 goto end
)
if %_TEST%==1 (
    call :test
    if not !_EXITCODE!==0 goto end
)
if %_SAVE%==1 (
    call :save
    if not !_EXITCODE!==0 goto end
)
goto end

@rem #########################################################################
@rem ## Subroutines

:env
set _BASENAME=%~n0
for %%f in ("%~dp0\..") do set "_ROOT_DIR=%%~dpf"
for %%f in ("%~dp0\.") do set "_PANDOC_DIR=%%~dpf"

set _DEBUG_LABEL=[DEBUG]
set _ERROR_LABEL=Error:

@rem see https://frightanic.com/computers/docker-default-container-names/
set _CONTAINER_NAME=scala3_docs
set _IMAGE_NAME=scala3/md2pdf
set _TAG_NAME=1.0

set "_DOCKER_CMD=docker.exe"
set "_GIT_CMD=git.exe"

set "_GZIP_CMD=gzip.exe"
where /q "%_GZIP_CMD%" 2>NUL
if %ERRORLEVEL%==1 (
    set "_GZIP_CMD=%GIT_HOME%\usr\bin\gzip.exe"
    if !ERRORLEVEL!==1 (
        echo %_ERROR_LABEL% Gzip executable not found 1>&2
        set _EXITCODE=1
        goto :eof
    )
)
set "_TARGET_DIR=%_ROOT_DIR%\out\pandoc"
set "_TAR_FILE=%_TARGET_DIR%\%_IMAGE_NAME:/=_%-%_TAG_NAME%.tar"
goto :eof

@rem input parameter: %*
:args
set _CLEAN=0
set _HELP=0
set _BUILD=0
set _PROJECT_NAME=internals
set _SAVE=0
set _TEST=0
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
    if "%__ARG%"=="build" ( set _BUILD=1
	) else if "%__ARG%"=="help" ( set _HELP=1
    ) else if "%__ARG%"=="save" ( set _SAVE=1
    ) else if "%__ARG%"=="test" ( set _TEST=1
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

set "_FILE_NAME=scala3_%_PROJECT_NAME%.pdf
set "_OUTPUT_FILE=%_TARGET_DIR%\%_FILE_NAME%"

if %_DEBUG%==1 (
    echo %_DEBUG_LABEL% Options    : _VERBOSE=%_VERBOSE% 1>&2
    echo %_DEBUG_LABEL% Subcommands: _BUILD=%_BUILD% _SAVE=%_SAVE% _TEST=%_TEST% 1>&2
)
goto :eof

:help
echo Usage: %_BASENAME% { ^<option^> ^| ^<subcommand^> }
echo.
echo   Options:
echo     -debug      show commands executed by this script
echo     -help       display this help message
echo     -verbose    display progress messages
echo.
echo   Subcommands:
echo     build       create image "%_IMAGE_NAME%:%_TAG_NAME%"
echo     help        display this help message
echo     save        save image as a tar archive file
echo     test        test image "%_IMAGE_NAME%:%_TAG_NAME%" with project %_PROJECT_NAME%
goto :eof

:build
for /f %%i in ('%_DOCKER_CMD% container ps --all --format "{{.Names}}" ^| findstr /c:"%_CONTAINER_NAME%"') do (
    if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_DOCKER_CMD%" container rm "%_CONTAINER_NAME%" 1>&2
    ) else if %_VERBOSE%==1 ( echo Remove container "%_CONTAINER_NAME%" based on image "%_IMAGE_NAME%" 1>&2
    )
    call "%_DOCKER_CMD%" container rm "%_CONTAINER_NAME%" %_REDIRECT_STDOUT%
    if not !ERRORLEVEL!==0 ( set _EXITCODE=1& goto :eof )
)
for /f %%i in ('%_DOCKER_CMD% image ls --format "{{.Repository}}" ^| findstr /c:"%_IMAGE_NAME%:%_TAG_NAME%"') do (
    if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_DOCKER_CMD%" image rm "%_IMAGE_NAME%:%_TAG_NAME%" 1>&2
    ) else if %_VERBOSE%==1 ( echo Remove docker image "%_IMAGE_NAME%:%_TAG_NAME%" 1>&2
    )
    call "%_DOCKER_CMD%" image rm "%_IMAGE_NAME%:%_TAG_NAME%" %_REDIRECT_STDOUT%
    if not !ERRORLEVEL!==0 ( set _EXITCODE=1& goto :eof )
)
@rem check path of Docker build context
if /i not "%CD%\"=="%_PANDOC_DIR%" (
    echo %_ERROR_LABEL% Docker build context must be the pandoc directory 1>&2
    set _EXITCODE=1
	goto :eof
)
xcopy /e /y "%_ROOT_DIR%docs\docs" "%_PANDOC_DIR%tmp\docs\" %_REDIRECT_STDOUT%
if not %ERRORLEVEL%==0 (
    echo %_ERROR_LABEL% Failed to copy directory docs\docs 1>&2
    set _EXITCODE=1
    goto :eof
)
set __BUILD_OPTS=--tag "%_IMAGE_NAME%:%_TAG_NAME%"

if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_DOCKER_CMD%" image build %__BUILD_OPTS% . 1>&2
) else if %_VERBOSE%==1 ( echo Build docker image "%_IMAGE_NAME%:%_TAG_NAME%" 1>&2
)
call "%_DOCKER_CMD%" image build %__BUILD_OPTS% . %_REDIRECT_STDOUT%
if not %ERRORLEVEL%==0 (
    set _EXITCODE=1
    goto :eof
)
goto :eof

:test
call :hash
if not %_EXITCODE%==0 goto :eof

@rem see https://docs.docker.com/engine/reference/commandline/run/
set __RUN_OPTS=--name "%_CONTAINER_NAME%"

if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_DOCKER_CMD%" container run %__RUN_OPTS% "%_IMAGE_NAME%:%_TAG_NAME%" 1>&2
) else if %_VERBOSE%==1 ( echo Start container "%_CONTAINER_NAME%" based on image "%_IMAGE_NAME%:%_TAG_NAME%" 1>&2
)
call "%_DOCKER_CMD%" container run %__RUN_OPTS% "%_IMAGE_NAME%:%_TAG_NAME%" %_REDIRECT_STDOUT%
if not %ERRORLEVEL%==0 ( set _EXITCODE=1& goto :eof )

if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_DOCKER_CMD%" cp "%_CONTAINER_NAME%:/dotty/out/pandoc/%_FILE_NAME%" "%_OUTPUT_FILE%" 1>&2
) else if %_VERBOSE%==1 ( echo Copy file %_FILE_NAME% to directory "!_TARGET_DIR:%_ROOT_DIR%\=!" 1>&2
)
call "%_DOCKER_CMD%" cp "%_CONTAINER_NAME%:/dotty/out/pandoc/%_FILE_NAME%" "%_OUTPUT_FILE%"
if not %ERRORLEVEL%==0 ( set _EXITCODE=1& goto :eof )

if %_DEBUG%==1 dir "%_TARGET_DIR%" 1>&2
goto :eof

@rem output parameter: _HASH
:hash
set _HASH=0000000000

set __GIT_URL=https://github.com/lampepfl/dotty.git
for /f "tokens=1,*" %%i in ('%_GIT_CMD% ls-remote "%__GIT_URL%" master') do set _HASH=%%i
set _HASH=%_HASH:~0,10%
goto :eof

:save
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_DOCKER_CMD%" save -o "%_TAR_FILE%" "%_IMAGE_NAME%:%_TAG_NAME%" 1>&2
) else if %_VERBOSE%==1 ( echo Save image "%_IMAGE_NAME%:%_TAG_NAME%" to archive file "!_TAR_FILE:%_ROOT_DIR%\=!" 1>&2
)
call "%_DOCKER_CMD%" save -o "%_TAR_FILE%" "%_IMAGE_NAME%:%_TAG_NAME%"
if not %ERRORLEVEL%==0 (
    set _EXITCODE=1
    goto :eof
)
if %_DEBUG%==1 ( echo %_DEBUG_LABEL% "%_GZIP_CMD%" --force --keep "%_TAR_FILE%" 1>&2
) else if %_VERBOSE%==1 ( echo Compress archive file "!_TAR_FILE:%_ROOT_DIR%\=!" 1>&2
) 
call "%_GZIP_CMD%" --force --keep "%_TAR_FILE%"
if not %ERRORLEVEL%==0 (
    set _EXITCODE=1
    goto :eof
)
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
if %_DEBUG%==1 echo %_DEBUG_LABEL% _EXITCODE=%_EXITCODE% 1>&2
exit /b %_EXITCODE%
endlocal
