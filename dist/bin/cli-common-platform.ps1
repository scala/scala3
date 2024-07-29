# We need to escape % in the java command path, for some reason this doesn't work in common.bat
#$_JAVACMD = $_JAVACMD -replace '%', '%%'

$SCALA_CLI_CMD_WIN = "`"$_JAVACMD`" -jar `"$_BIN_DIR/scala-cli.jar`""
