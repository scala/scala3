###################################################################################################
###                                   POWERSHELL SCALA SCRIPT                                   ###
###                                                                                             ###
### Author: Hamza REMMAL <hamza.remmal@epfl.ch>                                                 ###
### Since : Scala 3.5.0                                                                         ###
###################################################################################################

# Environment setup
$_PROG_HOME = $PSScriptRoot.TrimEnd('\bin\')
# Load and execute the common script
. "$_PROG_HOME/bin/common.ps1" # TODO: Will this work on Windows or not ?
# Fetch the version of Scala
$_SCALA_VERSION = Scala-FetchScalaVersion

# SCALA_CLI_CMD_WIN is an array, set in cli-common-platform.bat.
# WE NEED TO PASS '--skip-cli-updates' for JVM launchers but we actually don't need it for native launchers
& "$_JAVACMD" "-jar" "$_BIN_DIR/scala-cli.jar" "--prog-name" "scala" "--skip-cli-updates" "--cli-default-scala-version" "$_SCALA_VERSION" "-r" "$_MVN_REPOSITORY" $args

if ($LASTEXITCODE -ne 0) { exit 1 }
exit 0
