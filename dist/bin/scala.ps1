###################################################################################################
###                                   POWERSHELL SCALA SCRIPT                                   ###
###                                                                                             ###
### Author: Hamza REMMAL <hamza@remmal.dev>                                                     ###
### Since : Scala 3.6.0                                                                         ###
###################################################################################################

# Environment setup
$_PROG_HOME = $PSScriptRoot.TrimEnd('\bin\')
# Load and execute the common script
. "$_PROG_HOME/libexec/common.ps1"
. "$_LIBEXEC_DIR/cli-common-platform.ps1"

# WE NEED TO PASS '--skip-cli-updates' for JVM launchers but we actually don't need it for native launchers
& $SCALA_CLI_CMD_WIN "--prog-name" "scala" "--skip-cli-updates" "--cli-default-scala-version" "$(Get-FetchScalaVersion)" "-r" "$_MVN_REPOSITORY" $args

if ($LASTEXITCODE -ne 0) { exit 1 }
exit 0
