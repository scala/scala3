@echo off
setlocal

@rem paths are relative to the root project directory
set "_PREFIX=dist\win-x86_64\target\universal\stage"
set "_SOURCE=tests\pos\HelloWorld.scala"
set "_SOURCE_TEST_BOOTSTRAPPED=project\scripts\native-integration\testBootstrappedLibrary.scala"
set "_OUT_DIR=out"

@rem if-tests mimic the non-existing bash instruction 'set -e'.
call "%_PREFIX%\bin\scalac.bat" "@project\scripts\options" "%_SOURCE%"
if not %ERRORLEVEL%==0 endlocal& exit /b 1

@rem Create output directory (scalac requires it to exist)
if exist "%_OUT_DIR%" rmdir /s /q "%_OUT_DIR%"
mkdir "%_OUT_DIR%"

call "%_PREFIX%\bin\scalac.bat" -d "%_OUT_DIR%" "%_SOURCE%"
if not %ERRORLEVEL%==0 endlocal& exit /b 1

call "%_PREFIX%\bin\scala.bat" --power -classpath "%_OUT_DIR%" -M HelloWorld --offline --server=false
if not %ERRORLEVEL%==0 endlocal& exit /b 1

echo testing REPL direct invocation with -repl-init-script
call "%_PREFIX%\bin\scala.bat" -repl-init-script "println(\"REPL_INIT_OK\"); assert(scala.util.Properties.versionNumberString.startsWith(\"3.\"))" -repl-quit-after-init --power --offline --server=false 2>&1 | findstr /C:"REPL_INIT_OK" >nul
if not %ERRORLEVEL%==0 (
    echo REPL init script test failed: output did not contain REPL_INIT_OK
    endlocal& exit /b 1
)

echo testing bootstrapped standard library (scala-library-bootstrapped)
call "%_PREFIX%\bin\scala.bat" run "%_SOURCE_TEST_BOOTSTRAPPED%" --power --offline --server=false 2>&1 | findstr /C:"BOOTSTRAPPED_LIBRARY_TEST_PASSED" >nul
if not %ERRORLEVEL%==0 (
    echo Bootstrapped library test failed: output did not contain BOOTSTRAPPED_LIBRARY_TEST_PASSED
    endlocal& exit /b 1
)

echo All tests passed!
endlocal
