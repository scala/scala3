@echo off
setlocal

@rem paths are relative to the root project directory
set "_PREFIX=dist\win-x86_64\target\universal\stage"
set "_SOURCE=tests\pos\HelloWorld.scala"
set "_OUT_DIR=out"
set "_SITE_DIR=_site"

@rem if-tests mimic the non-existing bash instruction 'set -e'.
call "%_PREFIX%\bin\scalac.bat" "@project\scripts\options" "%_SOURCE%"
if not %ERRORLEVEL%==0 endlocal& exit /b 1

call "%_PREFIX%\bin\scalac.bat" -d "%_OUT_DIR%" "%_SOURCE%"
if not %ERRORLEVEL%==0 endlocal& exit /b 1

call "%_PREFIX%\bin\scala.bat" --power -classpath "%_OUT_DIR%" -M HelloWorld --offline --server=false
if not %ERRORLEVEL%==0 endlocal& exit /b 1

call "%_PREFIX%\bin\scala.bat" --power -classpath "%_OUT_DIR%" -J -Xmx512m -M HelloWorld --offline --server=false
if not %ERRORLEVEL%==0 endlocal& exit /b 1

if not exist "%_SITE_DIR%" mkdir "%_SITE_DIR%"
call "%_PREFIX%\bin\scaladoc.bat" -d "%_SITE_DIR%" -project Hello "%_SOURCE%"
endlocal
