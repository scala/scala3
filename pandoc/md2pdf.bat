@rem ## Created by mics (https://github.com/michelou/) on December 2020.

@echo off
setlocal enabledelayedexpansion

@rem #########################################################################
@rem ## Environment setup

set _EXITCODE=0

call :env
if not %_EXITCODE%==0 goto end

call :args %*
if not %_EXITCODE%==0 goto end

@rem #########################################################################
@rem ## Main

call :compile
if not %_EXITCODE%==0 goto end

goto end

@rem #########################################################################
@rem ## Subroutines

:env
set "_ROOT_DIR=%~dp0"

if not exist "%GIT_HOME%\usr\bin\sed.exe" (
    echo Error: Git installation not found ^(check variable GIT_HOME^) 1>&2
    set _EXITCODE=1
    goto :eof
)
set "_SED_CMD=%GIT_HOME%\usr\bin\sed.exe"

if not exist "%PANDOC_HOME%\pandoc.exe" (
    echo Error: Pandoc 2 installation not found ^(check variable PANDOC_HOME^) 1>&2
    set _EXITCODE=1
    goto :eof
)
set "_PANDOC_CMD=%PANDOC_HOME%\pandoc.exe"

@rem either 'lualatex.exe' or 'xelatex.exe' (both support UTF-8)
if not exist "%TEXLIVE_HOME%\bin\win32\lualatex.exe" (
    echo Error: TexLive installation not found ^(check variable TEXLIVE_HOME^) 1>&2
    set _EXITCODE=1
    goto :eof
)
set "__LATEX_CMD=%TEXLIVE_HOME%\bin\win32\lualatex.exe"

for %%f in ("%~dp0\.") do set "_SOURCE_DIR=%%~dpfdocs\docs"
set "_DATA_DIR=%_ROOT_DIR%data"
set "_IMAGES_DIR=%_DATA_DIR%\images"
set "_TEMPLATES_DIR=%_DATA_DIR%\templates"
for %%f in ("%~dp0\.") do set "_TARGET_DIR=%%~dpfout\pandoc"

if not exist "%_DATA_DIR%\defaults.yaml" (
    echo Error: Pandoc defaults file not found 1>&2
    set _EXITCODE=1
    goto :eof
)
set "__DEFAULTS_FILE=%_DATA_DIR%\defaults.yaml"

if not exist "%_TEMPLATES_DIR%\template.tex" (
    echo Error: Pandoc template file not found 1>&2
    set _EXITCODE=1
    goto :eof
)
set "__TEMPLATE_FILE=%_TEMPLATES_DIR%\template.tex"

if not exist "%_TEMPLATES_DIR%\scala.xml" (
    echo Error: Kate syntax file not found 1>&2
    set _EXITCODE=1
    goto :eof
)
set "__SYNTAX_FILE=%_TEMPLATES_DIR%\scala.xml"

set __SUBTITLE=Internal Draft
for /f "usebackq tokens=1,*" %%i in (`"%GIT_HOME%\bin\git.exe" ls-remote https://github.com/lampepfl/dotty.git master 2^>NUL`) do (
   set __HASH=%%i
   set "__SUBTITLE=%__SUBTITLE% (rev !__HASH:~0,10!^)"
)
for /f "usebackq delims=" %%d in (`powershell -c "$culture=[CultureInfo]'en-us'; (Get-Date).ToString('dd MMMM yyyy', $culture)"`) do set "__TODAY=%%d"

@rem set _PANDOC_OPTS=--verbose
set _PANDOC_OPTS=--quiet
set _PANDOC_OPTS=%_PANDOC_OPTS% --data-dir="%_DATA_DIR%"
set _PANDOC_OPTS=%_PANDOC_OPTS% --defaults="%__DEFAULTS_FILE%"
@rem CAUTION: option --syntax-definition does not work with absolute paths
set _PANDOC_OPTS=%_PANDOC_OPTS% --syntax-definition="!__SYNTAX_FILE:%_ROOT_DIR%=!"
set _PANDOC_OPTS=%_PANDOC_OPTS% -V geometry=a4paper -V geometry:margin=30mm
@rem DejaVuSerif font fails to display greek characters correctly (e.g. β-reduce)
@rem set _PANDOC_OPTS=%_PANDOC_OPTS% -V "mainfont:DejaVuSerif.ttf"
@rem set _PANDOC_OPTS=%_PANDOC_OPTS% -V "mainfont:FreeSerif.ttf"
@rem set _PANDOC_OPTS=%_PANDOC_OPTS% -V "sansfont:DejaVuSans.ttf"
@rem set _PANDOC_OPTS=%_PANDOC_OPTS% -V monofont:DejaVuSansMono.ttf
@rem set _PANDOC_OPTS=%_PANDOC_OPTS% -V mathfont:texgyredejavu-math.otf
set _PANDOC_OPTS=%_PANDOC_OPTS% -V fontsize=12pt
@rem see https://www.htmlcsscolor.com/hex/66001E
set _PANDOC_OPTS=%_PANDOC_OPTS% -V urlcolor=blue -V linkcolor="[HTML]{66001E}"
set _PANDOC_OPTS=%_PANDOC_OPTS% -V subtitle="%__SUBTITLE%" -V date="%__TODAY%"
set _PANDOC_OPTS=%_PANDOC_OPTS% --template="%__TEMPLATE_FILE%"
set _PANDOC_OPTS=%_PANDOC_OPTS% --pdf-engine="%__LATEX_CMD%"
goto :eof

@rem input parameter: %*
@rem output parameters: _INPUT_FILE, _OUTPUT_FILE
:args
set "__ARG=%~1"
if not defined __ARG ( set __PROJECT_NAME=reference
) else if "%__ARG%"=="reference" ( set __PROJECT_NAME=reference
) else if "%__ARG%"=="internals" ( set __PROJECT_NAME=internals
) else if "%__ARG%"=="usage" ( set __PROJECT_NAME=usage
) else (
    echo Error: Unknown project %__ARG% ^(expected: internals, reference or usage^) 1>&2
    set _EXITCODE=1
    goto :eof
)
if not exist "%_DATA_DIR%\%__PROJECT_NAME%.md" (
    echo Error: Main Markdown file not found 1>&2
    set _EXITCODE=1
    goto :eof
)
set "_INPUT_FILE=%_DATA_DIR%\%__PROJECT_NAME%.md"

set "_OUTPUT_FILE=%_TARGET_DIR%\scala3_%__PROJECT_NAME%.pdf"
set _PANDOC_OPTS=%_PANDOC_OPTS% --output="%_OUTPUT_FILE%"
goto :eof

:compile
call :compile_images
if not %_EXITCODE%==0 goto :eof

call :compile_anchors
if not %_EXITCODE%==0 goto :eof

call "%_PANDOC_CMD%" %_PANDOC_OPTS% "%_INPUT_FILE%"
if not %ERRORLEVEL%==0 (
    set _EXITCODE=1
    goto :eof
)
goto :eof

:compile_images
set "__TARGET_IMAGES_DIR=%_TARGET_DIR%\images"

xcopy /s /y "%_IMAGES_DIR%\*" "%__TARGET_IMAGES_DIR%\" 1>NUL
if not %ERRORLEVEL%==0 (
    echo Error: Failed to copy image files to directory "%__TARGET_IMAGES_DIR%" 1>&2
    set _EXITCODE=1
    goto :eof
)
goto :eof

@rem https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators#replacement-operator
:compile_anchors
set "__SRC_DIR=%_SOURCE_DIR%\%_PROJECT_DIR%"
set "__SRC_MANAGED_DIR=%_TARGET_DIR%\src_managed\%_PROJECT_DIR%"
if not exist "%__SRC_MANAGED_DIR%" mkdir "%__SRC_MANAGED_DIR%"

for /f "usebackq delims=" %%f in (`dir /s /b "%__SRC_DIR%\*.md"`) do (
    set "__IN_FILE=%%f"
    set "__OUT_FILE=!__IN_FILE:%__SRC_DIR%=%__SRC_MANAGED_DIR%!"
    for %%i in ("!__OUT_FILE!") do set "__OUT_DIR=%%~dpi"
    if not exist "!__OUT_DIR!" mkdir "!__OUT_DIR!"

    @rem We specifies 2 regexs in case another anchor exists on same line
    call "%_SED_CMD%" -e "s/\[\(.\^+\)\](\(.*\/\)*\(.\^+\)\.md)/[\1](#\3)/g" -e "s/\[\(.\^+\)\](\(.*\/\)*\(.\^+\)\.md)/[\1](#\3)/g" "!__IN_FILE!" > "!__OUT_FILE!"
    if not !ERRORLEVEL!==0 (
        set _EXITCODE=1
        goto :eof
    )
    @rem Markdown file contains image urls
    if "!__OUT_FILE:~-9!"=="-nulls.md" (
        set "__TMP_FILE=!__OUT_FILE!.tmp"
        copy "!__OUT_FILE!" "!__TMP_FILE!" 1>NUL

        @rem e.g. ![](../../../images/image.png "Image")
        call "%_SED_CMD%" "s/\^!\[\(.*\)\](\(\.\.\/\)*\(.*\))/^![\1](\.\.\/out\/pandoc\/\3)/g" "!__TMP_FILE!" > "!__OUT_FILE!"
        if not !ERRORLEVEL!==0 (
            set _EXITCODE=1
            goto :eof
        )
    )
)
goto :eof

@rem #########################################################################
@rem ## Cleanups

:end
exit /b %_EXITCODE%
endlocal
