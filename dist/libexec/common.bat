@rem #########################################################################
@rem ## Code common to scalac.bat, scaladoc.bat and scala.bat

if defined JAVACMD (
    set "_JAVACMD=%JAVACMD%"
) else if defined JAVA_HOME (
    set "_JAVACMD=%JAVA_HOME%\bin\java.exe"
) else if defined JDK_HOME (
    set "_JAVACMD=%JDK_HOME%\bin\java.exe"
) else (
    where /q java.exe
    if !ERRORLEVEL!==0 (
        set __JAVA_BIN_DIR=
        for /f "delims=" %%i in ('where /f java.exe') do (
            set "__PATH=%%~dpi"
            @rem we take first occurrence and ignore Oracle path for java executable
            if not defined __JAVA_BIN_DIR if "!__PATH!"=="!__PATH:javapath=!" set "__JAVA_BIN_DIR=!__PATH!"
        )
        if defined __JAVA_BIN_DIR set "_JAVACMD=!__JAVA_BIN_DIR!\java.exe"
    )
    if not defined _JAVACMD (
        set "__PATH=%ProgramFiles%\Java"
        for /f %%f in ('dir /ad /b "!__PATH!\jre*" 2^>NUL') do set "_JAVA_HOME=!__PATH!\%%f"
        if not defined _JAVA_HOME (
           set __PATH=C:\opt
           for /f %%f in ('dir /ad /b "!__PATH!\jdk*" 2^>NUL') do set "_JAVA_HOME=!__PATH!\%%f\jre"
        )
        if defined _JAVA_HOME set "_JAVACMD=!_JAVA_HOME!\bin\java.exe"
    )
)
if not exist "%_JAVACMD%" (
   echo Error: Java executable not found ^(!_JAVACMD!^) 1>&2
   set _EXITCODE=1
   goto :eof
)
if not defined _PROG_HOME (
   echo Error: Variable _PROG_HOME undefined 1>&2
   set _EXITCODE=1
   goto :eof
)
set "_LIB_DIR=%_PROG_HOME%\lib"

set _PSEP=;
