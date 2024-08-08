###################################################################################################
###                                  POWERSHELL SCALAC SCRIPT                                   ###
###                                                                                             ###
### Author: Hamza REMMAL <hamza.remmal@epfl.ch>                                                 ###
### Since : Scala 3.5.0                                                                         ###
###################################################################################################

# Environment setup
$_PROG_HOME = $PSScriptRoot.TrimEnd('\bin\')
# Load and execute the common script
. "$_PROG_HOME/bin/common.ps1"

###################################################################################################
############################################ FUNCTIONS ############################################
###################################################################################################

function Scala-ArgumentParsing {
    param ( [string[]] $params )

    $_JAVA_ARGS = ""                  # TODO: THIS SHOULD BE AN ARRAY
    $_SCALA_ARGS = ""                 # TODO: THIS SHOULD BE AN ARRAY
    $_SCALA_CLASSPATH = ""
    $_CONSUME_REMAINING = $false

    while ($params.Count -gt 0) {
        $arg = $params[0]
        if ($_CONSUME_REMAINING) {
            $_SCALA_ARGS += " $arg"
            $params = $params[1..$params.Length]
        } elseif ($arg -eq "--" -or $arg -eq "-script") {
            $_CONSUME_REMAINING = $true
            $_SCALA_ARGS += " $arg"
            $params = $params[1..$params.Length]
        } elseif ($arg -eq "-Oshort") {
            $_JAVA_ARGS  += " -XX:+TieredCompilation -XX:TieredStopAtLevel=1"
            $_SCALA_ARGS += " -Oshort"
            $params = $params[1..$params.Length]
        } elseif ($arg.StartsWith("-D")) {
            $_JAVA_ARGS  += " $arg"
            $_SCALA_ARGS += " $arg"
            $params = $params[1..$params.Length]
        } elseif ($arg.StartsWith("-J")) {
            $_JAVA_ARGS  += " ${arg:2}"
            $_SCALA_ARGS += " $arg"
            $params = $params[1..$params.Length]
        } elseif ($arg -eq "-classpath" -or $arg -eq "-cp") {
            $_SCALA_CLASSPATH = $params[1]
            $params = $params[2..$params.Length]
        } else {
            $_SCALA_ARGS += " $arg"
            $params = $params[1..$params.Length]
        }
    }

    return @{
        JAVA_ARGS       = $_JAVA_ARGS
        SCALA_ARGS      = $_SCALA_ARGS
        SCALA_CLASSPATH = $_SCALA_CLASSPATH
    }

}

function Scala-LoadCompilerClasspath {
    param ( [string] $SCALA_CLASSPATH )

    $__SCALA_JAR = Join-Path $_LIB_DIR "scala.jar"
    $__WITH_COMPILER_JAR = Join-Path $_LIB_DIR with_compiler.jar

    $__TOOLCHAIN = "$__SCALA_JAR$_PSEP$__WITH_COMPILER_JAR"

    if ($SCALA_CLASSPATH) {
        $__TOOLCHAIN += "$_PSEP$SCALA_CLASSPATH"
    }
    return $__TOOLCHAIN
}


###################################################################################################
############################################## SCRIPT #############################################
###################################################################################################

# Parse the arguments
$_ARG_RESULT = Scala-ArgumentParsing $args

# Compute the classpath
$_CLASS_PATH = Scala-LoadCompilerClasspath $_ARG_RESULT.SCALA_CLASSPATH

# Fetch the arguments
$_JAVA_ARGS  = $_ARG_RESULT.JAVA_ARGS
$_SCALA_ARGS = $_ARG_RESULT.SCALA_ARGS

# Build the java arguments
$command = @()
if (-not [string]::IsNullOrEmpty($_JAVA_ARGS)) { $command += $_JAVA_ARGS }
$command += "-classpath"
$command += $_CLASS_PATH
$command += "-Dscala.usejavacp=true"
$command += "-Dscala.home=$_PROG_HOME"
$command += "dotty.tools.MainGenericCompiler"

if (-not [string]::IsNullOrEmpty($_SCALA_ARGS)) { $command += $_SCALA_ARGS }
$commandString = $command -join ' '

Start-Process -FilePath $_JAVACMD -ArgumentList $commandString -NoNewWindow -Wait

if ($LASTEXITCODE -ne 0) { exit 1 }

exit 0
