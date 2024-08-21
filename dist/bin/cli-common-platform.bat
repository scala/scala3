@echo off

@rem we need to escape % in the java command path, for some reason this doesnt work in common.bat
set "_JAVACMD=!_JAVACMD:%%=%%%%!"
set SCALA_CLI_CMD_WIN="%_JAVACMD%" "-jar" "%_PROG_HOME%\bin\scala-cli.jar"