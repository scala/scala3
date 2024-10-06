###################################################################################################
###                                 POWERSHELL SCALADOC SCRIPT                                  ###
###                                                                                             ###
### Author: Hamza REMMAL <hamza@remmal.dev>                                                     ###
### Since : Scala 3.6.0                                                                         ###
###################################################################################################

# Environment setup
$_PROG_HOME = $PSScriptRoot.TrimEnd('\bin\')

. "$_PROG_HOME/libexec/common.ps1"

$_DEFAULT_JAVA_OPTS = "-Xmx768m -Xms768m"

###################################################################################################
############################################ FUNCTIONS ############################################
###################################################################################################

function Get-ScaladocArgumentParsing {
    param ( [string[]] $params )

    $_JAVA_DEBUG = ""
    $_VERBOSE = $false
    $_QUIET = $false
    $_COLORS = $false
    $_SCALA_ARGS = @()
    $_JAVA_ARGS = @()
    $_RESIDUAL_ARGS = @()
    $_IN_SCRIPTING_ARGS = $false

    while ($params.Count -gt 0) {
        $arg = $params[0]
        switch ($arg) {
            "--" {
                $_IN_SCRIPTING_ARGS = $true
            }
            "-h" {
                $_SCALA_ARGS += "-help"
            }
            "-help" {
                $_SCALA_ARGS += "-help"
            }
            "-v" {
                $_VERBOSE = $true
                $_SCALA_ARGS += "-verbose"
            }
            "-verbose" {
                $_VERBOSE = $true
                $_SCALA_ARGS += "-verbose"
            }
            "-debug" {
                $_JAVA_DEBUG = $_DEBUG_STR
            }
            "-q" {
                $_QUIET = $true
            }
            "-quiet" {
                $_QUIET = $true
            }
            "-colors" {
                $_COLORS = $true
            }
            "-no-colors" {
                $_COLORS = $false
            }
            default {
                if ($arg.StartsWith("-D")) {
                    $_JAVA_ARGS += "$arg"
                } elseif ($arg.StartsWith("-J")) {
                    $_JAVA_ARGS += "${arg:2}"
                } else {
                    if ($_IN_SCRIPTING_ARGS) {
                    } else {
                        $_RESIDUAL_ARGS += "$arg"
                    }
                }
            }
        }
        $params = $params[1..$params.Length]
    }

    return @{
        JAVA_DEBUG    = $_JAVA_DEBUG
        SCALA_ARGS    = $_SCALA_ARGS
        JAVA_ARGS     = $_JAVA_ARGS
        RESIDUAL_ARGS = $_RESIDUAL_ARGS
    }
}

###################################################################################################
############################################## SCRIPT #############################################
###################################################################################################


$scaladocParameters = Get-ScaladocArgumentParsing $args

# Main

if ($JAVA_OPTS) {
    $_JAVA_OPTS = $JAVA_OPTS
} else {
    $_JAVA_OPTS = $_DEFAULT_JAVA_OPTS
}


$_JAVA_DEBUG = $scaladocParameters.JAVA_DEBUG

# Build the java arguments
$command = @()
if (-not [string]::IsNullOrEmpty($_JAVA_OPTS)) { $command += $_JAVA_OPTS }
if (-not [string]::IsNullOrEmpty($_JAVA_DEBUG)) { $command += $_JAVA_DEBUG }
$command += $scaladocParameters.JAVA_ARGS
$command += "-classpath"
$command += "$_LIB_DIR/scaladoc.jar"
$command += "-Dscala.expandjavacp=true"
$command += "-Dscala.usejavacp=true"
$command += "-Dscala.home=$_PROG_HOME"
$command += "dotty.tools.scaladoc.Main"
$command += $scaladocParameters.SCALA_ARGS
$command += $scaladocParameters.RESIDUAL_ARGS

$_JAVA_PROCESS = Start-Process -FilePath $_JAVACMD -ArgumentList $($command -join ' ') -NoNewWindow -PassThru -Wait

if ($_JAVA_PROCESS.ExitCode -ne 0) { exit 1 }

exit 0
