if defined JAVACMD (
    set _JAVACMD=%JAVACMD%
    if %_DEBUG%==1 echo [%_BASENAME%] Using environment variable JAVACMD
) else if defined JAVA_HOME (
    set _JAVACMD=%JAVA_HOME%\bin\java.exe
    if %_DEBUG%==1 echo [%_BASENAME%] Using environment variable JAVA_HOME
) else if defined JDK_HOME (
    set _JAVACMD=%JDK_HOME%\bin\java.exe
    if %_DEBUG%==1 echo [%_BASENAME%] Using environment variable JDK_HOME
) else (
    where /q java.exe
    if !ERRORLEVEL!==0 (
        for /f "delims=" %%i in ('where /f java.exe') do set _JAVA_BIN_DIR=%%~dpsi
        rem we ignore Oracle path for java executable
        if "!_JAVA_BIN_DIR!"=="!_JAVA_BIN_DIR:javapath=!" set _JAVACMD=!_JAVA_BIN_DIR!\java.exe
    )
    if not defined _JAVACMD (
        set _PATH=C:\Progra~1\Java
        for /f %%f in ('dir /ad /b "!_PATH!\jre*" 2^>NUL') do set _JAVA_HOME=!_PATH!\%%f
        if not defined _JAVA_HOME (
           set _PATH=C:\opt
           for /f %%f in ('dir /ad /b "!_PATH!\jdk*" 2^>NUL') do set _JAVA_HOME=!_PATH!\%%f\jre
        )
        if defined _JAVA_HOME (
            if %_DEBUG%==1 echo [%_BASENAME%] Using default Java installation directory !_JAVA_HOME!
            set _JAVACMD=!_JAVA_HOME!\bin\java.exe
        )
    )
)
if not exist "%_JAVACMD%" (
   if %_DEBUG%==1 echo [%_BASENAME%] Error: Java executable not found ^(%_JAVACMD%^)
   set _EXITCODE=1
   goto :eof
)

if defined DOTTY_HOME (
    set _LIB_DIR=%DOTTY_HOME%\lib
) else (
    if not defined _PROG_HOME (
        for %%f in ("%~dp0..") do set _PROG_HOME=%%~sf
    )
    set _LIB_DIR=!_PROG_HOME!\lib
)

set _PSEP=;

for /f %%f in ('dir /b "%_LIB_DIR%\*dotty-compiler*"')       do set _DOTTY_COMP=%_LIB_DIR%\%%f
for /f %%f in ('dir /b "%_LIB_DIR%\*dotty-interfaces*"')     do set _DOTTY_INTF=%_LIB_DIR%\%%f
for /f %%f in ('dir /b "%_LIB_DIR%\*dotty-library*"')        do set _DOTTY_LIB=%_LIB_DIR%\%%f
for /f %%f in ('dir /b "%_LIB_DIR%\*scala-asm*"')            do set _SCALA_ASM=%_LIB_DIR%\%%f
for /f %%f in ('dir /b "%_LIB_DIR%\*scala-library*"')        do set _SCALA_LIB=%_LIB_DIR%\%%f
for /f %%f in ('dir /b "%_LIB_DIR%\*scala-xml*"')            do set _SCALA_XML=%_LIB_DIR%\%%f
for /f %%f in ('dir /b "%_LIB_DIR%\*compiler-interface*"')   do set _SBT_INTF=%_LIB_DIR%\%%f
for /f %%f in ('dir /b "%_LIB_DIR%\*jline-reader-3*"')       do set _JLINE_READER=%_LIB_DIR%\%%f
for /f %%f in ('dir /b "%_LIB_DIR%\*jline-terminal-3*"')     do set _JLINE_TERMINAL=%_LIB_DIR%\%%f
for /f %%f in ('dir /b "%_LIB_DIR%\*jline-terminal-jna-3*"') do set _JLINE_TERMINAL_JNA=%_LIB_DIR%\%%f
for /f %%f in ('dir /b "%_LIB_DIR%\*jna-4*"')                do set _JNA=%_LIB_DIR%\%%f

rem debug
set _DEBUG_STR=-agentlib:jdwp=transport=dt_socket,server=y,suspend=y,address=5005
