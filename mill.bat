@echo off

setlocal enabledelayedexpansion

if [!DEFAULT_MILL_VERSION!]==[] ( set "DEFAULT_MILL_VERSION=1.1.4" )

if [!MILL_GITHUB_RELEASE_CDN!]==[] ( set "MILL_GITHUB_RELEASE_CDN=" )

if [!MILL_MAIN_CLI!]==[] ( set "MILL_MAIN_CLI=%~f0" )

set "MILL_REPO_URL=https://github.com/com-lihaoyi/mill"

SET MILL_BUILD_SCRIPT=

if exist "build.mill" (
  set MILL_BUILD_SCRIPT=build.mill
) else (
    if exist "build.mill.scala" (
      set MILL_BUILD_SCRIPT=build.mill.scala
    ) else (
        if exist "build.sc" (
          set MILL_BUILD_SCRIPT=build.sc
        ) else (
            rem no-op
        )
    )
)

if [!MILL_VERSION!]==[] (
  if exist .mill-version (
    set /p MILL_VERSION=<.mill-version
  ) else (
    if exist .config\mill-version (
      set /p MILL_VERSION=<.config\mill-version
    ) else (
      rem Determine which config file to use for version extraction
      set "MILL_VERSION_CONFIG_FILE="
      set "MILL_VERSION_SEARCH_PATTERN="

      if exist build.mill.yaml (
        set "MILL_VERSION_CONFIG_FILE=build.mill.yaml"
        set "MILL_VERSION_SEARCH_PATTERN=mill-version:"
      ) else (
        if not "%MILL_BUILD_SCRIPT%"=="" (
          set "MILL_VERSION_CONFIG_FILE=%MILL_BUILD_SCRIPT%"
          set "MILL_VERSION_SEARCH_PATTERN=//\|.*mill-version"
        )
      )

      rem Process the config file if found
      if not "!MILL_VERSION_CONFIG_FILE!"=="" (
        rem Find the line and process it
        for /f "tokens=*" %%a in ('findstr /R /C:"!MILL_VERSION_SEARCH_PATTERN!" "!MILL_VERSION_CONFIG_FILE!"') do (
            set "line=%%a"

            rem --- 1. Replicate sed 's/.*://' ---
            rem This removes everything up to and including the first colon
            set "line=!line:*:=!"

            rem --- 2. Replicate sed 's/#.*//' ---
            rem Split on '#' and keep the first part
            for /f "tokens=1 delims=#" %%b in ("!line!") do (
                set "line=%%b"
            )

            rem --- 3. Replicate sed 's/['"]//g' ---
            rem Remove all quotes
            set "line=!line:'=!"
            set "line=!line:"=!"

            rem --- 4. Replicate sed's trim/space removal ---
            rem Remove all space characters from the result. This is more robust.
            set "MILL_VERSION=!line: =!"

            rem We found the version, so we can exit the loop
            goto :version_found
        )

        :version_found
        rem no-op
      )
    )
  )
)

if [!MILL_VERSION!]==[] (
    set MILL_VERSION=%DEFAULT_MILL_VERSION%
)

if [!MILL_FINAL_DOWNLOAD_FOLDER!]==[] set MILL_FINAL_DOWNLOAD_FOLDER=%USERPROFILE%\.cache\mill\download

rem without bat file extension, cmd doesn't seem to be able to run it

set "MILL_NATIVE_SUFFIX=-native"
set "MILL_JVM_SUFFIX=-jvm"
set "MILL_FULL_VERSION=%MILL_VERSION%"
set "MILL_DOWNLOAD_EXT=.bat"
set "ARTIFACT_SUFFIX="
REM Check if MILL_VERSION contains MILL_NATIVE_SUFFIX
echo !MILL_VERSION! | findstr /C:"%MILL_NATIVE_SUFFIX%" >nul
if !errorlevel! equ 0 (
    set "MILL_VERSION=%MILL_VERSION:-native=%"
    REM -native images compiled with graal do not support windows-arm
    REM https://github.com/oracle/graal/issues/9215
    IF /I NOT "%PROCESSOR_ARCHITECTURE%"=="ARM64" (
        set "ARTIFACT_SUFFIX=-native-windows-amd64"
        set "MILL_DOWNLOAD_EXT=.exe"
    ) else (
        rem no-op
    )
) else (
    echo !MILL_VERSION! | findstr /C:"%MILL_JVM_SUFFIX%" >nul
    if !errorlevel! equ 0 (
        set "MILL_VERSION=%MILL_VERSION:-jvm=%"
    ) else (
        set "SKIP_VERSION=false"
        set "MILL_PREFIX=%MILL_VERSION:~0,4%"
        if "!MILL_PREFIX!"=="0.1." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.2." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.3." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.4." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.5." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.6." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.7." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.8." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.9." set "SKIP_VERSION=true"
        set "MILL_PREFIX=%MILL_VERSION:~0,5%"
        if "!MILL_PREFIX!"=="0.10." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.11." set "SKIP_VERSION=true"
        if "!MILL_PREFIX!"=="0.12." set "SKIP_VERSION=true"

        if "!SKIP_VERSION!"=="false" (
            IF /I NOT "%PROCESSOR_ARCHITECTURE%"=="ARM64" (
                set "ARTIFACT_SUFFIX=-native-windows-amd64"
                set "MILL_DOWNLOAD_EXT=.exe"
            )
        ) else (
            rem no-op
        )
    )
)

set MILL=%MILL_FINAL_DOWNLOAD_FOLDER%\!MILL_FULL_VERSION!!MILL_DOWNLOAD_EXT!

set MILL_RESOLVE_DOWNLOAD=

if not exist "%MILL%" (
  set MILL_RESOLVE_DOWNLOAD=true
) else (
    if defined MILL_TEST_DRY_RUN_LAUNCHER_SCRIPT (
        set MILL_RESOLVE_DOWNLOAD=true
    ) else (
        rem no-op
    )
)


if [!MILL_RESOLVE_DOWNLOAD!]==[true] (
    set MILL_VERSION_PREFIX=%MILL_VERSION:~0,4%
    set MILL_SHORT_VERSION_PREFIX=%MILL_VERSION:~0,2%
    rem Since 0.5.0
    set MILL_DOWNLOAD_SUFFIX=-assembly
    rem Since 0.11.0
    set MILL_DOWNLOAD_FROM_MAVEN=1
    if [!MILL_VERSION_PREFIX!]==[0.0.] (
        set MILL_DOWNLOAD_SUFFIX=
        set MILL_DOWNLOAD_FROM_MAVEN=0
    )
    if [!MILL_VERSION_PREFIX!]==[0.1.] (
        set MILL_DOWNLOAD_SUFFIX=
        set MILL_DOWNLOAD_FROM_MAVEN=0
    )
    if [!MILL_VERSION_PREFIX!]==[0.2.] (
        set MILL_DOWNLOAD_SUFFIX=
        set MILL_DOWNLOAD_FROM_MAVEN=0
    )
    if [!MILL_VERSION_PREFIX!]==[0.3.] (
        set MILL_DOWNLOAD_SUFFIX=
        set MILL_DOWNLOAD_FROM_MAVEN=0
    )
    if [!MILL_VERSION_PREFIX!]==[0.4.] (
        set MILL_DOWNLOAD_SUFFIX=
        set MILL_DOWNLOAD_FROM_MAVEN=0
    )
    if [!MILL_VERSION_PREFIX!]==[0.5.] set MILL_DOWNLOAD_FROM_MAVEN=0
    if [!MILL_VERSION_PREFIX!]==[0.6.] set MILL_DOWNLOAD_FROM_MAVEN=0
    if [!MILL_VERSION_PREFIX!]==[0.7.] set MILL_DOWNLOAD_FROM_MAVEN=0
    if [!MILL_VERSION_PREFIX!]==[0.8.] set MILL_DOWNLOAD_FROM_MAVEN=0
    if [!MILL_VERSION_PREFIX!]==[0.9.] set MILL_DOWNLOAD_FROM_MAVEN=0

    set MILL_VERSION_PREFIX=%MILL_VERSION:~0,5%
    if [!MILL_VERSION_PREFIX!]==[0.10.] set MILL_DOWNLOAD_FROM_MAVEN=0

    set MILL_VERSION_PREFIX=%MILL_VERSION:~0,8%
    if [!MILL_VERSION_PREFIX!]==[0.11.0-M] set MILL_DOWNLOAD_FROM_MAVEN=0

    set MILL_VERSION_PREFIX=%MILL_VERSION:~0,5%
    set DOWNLOAD_EXT=exe
    if [!MILL_SHORT_VERSION_PREFIX!]==[0.] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION_PREFIX!]==[0.12.] set DOWNLOAD_EXT=exe
    if [!MILL_VERSION!]==[0.12.0] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.1] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.2] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.3] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.4] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.5] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.6] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.7] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.8] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.9] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.10] set DOWNLOAD_EXT=jar
    if [!MILL_VERSION!]==[0.12.11] set DOWNLOAD_EXT=jar

    set MILL_VERSION_PREFIX=
    set MILL_SHORT_VERSION_PREFIX=

    for /F "delims=- tokens=1" %%A in ("!MILL_VERSION!") do set MILL_VERSION_BASE=%%A
    set MILL_VERSION_MILESTONE=
    for /F "delims=- tokens=2" %%A in ("!MILL_VERSION!") do set MILL_VERSION_MILESTONE=%%A
    set MILL_VERSION_MILESTONE_START=!MILL_VERSION_MILESTONE:~0,1!
    if [!MILL_VERSION_MILESTONE_START!]==[M] (
        set MILL_VERSION_TAG=!MILL_VERSION_BASE!-!MILL_VERSION_MILESTONE!
    ) else (
        set MILL_VERSION_TAG=!MILL_VERSION_BASE!
    )
    if [!MILL_DOWNLOAD_FROM_MAVEN!]==[1] (
        set MILL_DOWNLOAD_URL=https://repo1.maven.org/maven2/com/lihaoyi/mill-dist!ARTIFACT_SUFFIX!/!MILL_VERSION!/mill-dist!ARTIFACT_SUFFIX!-!MILL_VERSION!.!DOWNLOAD_EXT!
    ) else (
        set MILL_DOWNLOAD_URL=!MILL_GITHUB_RELEASE_CDN!%MILL_REPO_URL%/releases/download/!MILL_VERSION_TAG!/!MILL_VERSION!!MILL_DOWNLOAD_SUFFIX!
    )

    if defined MILL_TEST_DRY_RUN_LAUNCHER_SCRIPT (
        echo !MILL_DOWNLOAD_URL!
        echo !MILL!
        exit /b 0
    )

    rem there seems to be no way to generate a unique temporary file path (on native Windows)
    if defined MILL_OUTPUT_DIR (
        set MILL_TEMP_DOWNLOAD_FILE=%MILL_OUTPUT_DIR%\mill-temp-download
        if not exist "%MILL_OUTPUT_DIR%" mkdir "%MILL_OUTPUT_DIR%"
    ) else (
        set MILL_TEMP_DOWNLOAD_FILE=out\mill-bootstrap-download
        if not exist "out" mkdir "out"
    )

    echo Downloading mill !MILL_VERSION! from !MILL_DOWNLOAD_URL! ... 1>&2

    curl -f -L "!MILL_DOWNLOAD_URL!" -o "!MILL_TEMP_DOWNLOAD_FILE!"

    if not exist "%MILL_FINAL_DOWNLOAD_FOLDER%" mkdir "%MILL_FINAL_DOWNLOAD_FOLDER%"
    move /y "!MILL_TEMP_DOWNLOAD_FILE!" "%MILL%"

    set MILL_TEMP_DOWNLOAD_FILE=
    set MILL_DOWNLOAD_SUFFIX=
)

set MILL_FINAL_DOWNLOAD_FOLDER=
set MILL_VERSION=
set MILL_REPO_URL=

rem Need to preserve the first position of those listed options
set MILL_FIRST_ARG=
if [%~1%]==[--bsp] (
  set MILL_FIRST_ARG=%1%
) else (
  if [%~1%]==[-i] (
    set MILL_FIRST_ARG=%1%
  ) else (
    if [%~1%]==[--interactive] (
      set MILL_FIRST_ARG=%1%
    ) else (
      if [%~1%]==[--no-server] (
        set MILL_FIRST_ARG=%1%
      ) else (
        if [%~1%]==[--no-daemon] (
          set MILL_FIRST_ARG=%1%
        ) else (
          if [%~1%]==[--help] (
            set MILL_FIRST_ARG=%1%
          )
        )
      )
    )
  )
)
set "MILL_PARAMS=%*%"

if not [!MILL_FIRST_ARG!]==[] (
  for /f "tokens=1*" %%a in ("%*") do (
    set "MILL_PARAMS=%%b"
  )
)

rem -D mill.main.cli is for compatibility with Mill 0.10.9 - 0.13.0-M2
"%MILL%" %MILL_FIRST_ARG% -D "mill.main.cli=%MILL_MAIN_CLI%" %MILL_PARAMS%
