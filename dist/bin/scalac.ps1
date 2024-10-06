###################################################################################################
###                                  POWERSHELL SCALAC SCRIPT                                   ###
###                                                                                             ###
### Author: Hamza REMMAL <hamza@remmal.dev>                                                     ###
### Since : Scala 3.6.0                                                                         ###
###################################################################################################

# Environment setup
$_PROG_HOME = $PSScriptRoot.TrimEnd('\bin\')
# Load and execute the common script
. "$_PROG_HOME/libexec/common.ps1"

###################################################################################################
############################################ FUNCTIONS ############################################
###################################################################################################

function Get-ScalaArgumentParsing {
    param ( [string[]] $params )

    $_JAVA_ARGS = @()
    $_SCALA_ARGS = @()
    $_SCALA_CLASSPATH = ""
    $_CONSUME_REMAINING = $false

    while ($params.Count -gt 0) {
        $arg = $params[0]
        if ($_CONSUME_REMAINING) {
            $_SCALA_ARGS += "$arg"
            $params = $params[1..$params.Length]
        } elseif ($arg -eq "--" -or $arg -eq "-script") {
            $_CONSUME_REMAINING = $true
            $_SCALA_ARGS += "$arg"
            $params = $params[1..$params.Length]
        } elseif ($arg -eq "-Oshort") {
            $_JAVA_ARGS  += "-XX:+TieredCompilation"
            $_JAVA_ARGS  += "-XX:TieredStopAtLevel=1"
            $_SCALA_ARGS += "-Oshort"
            $params = $params[1..$params.Length]
        } elseif ($arg.StartsWith("-D")) {
            $_JAVA_ARGS  += "$arg"
            $_SCALA_ARGS += "$arg"
            $params = $params[1..$params.Length]
        } elseif ($arg.StartsWith("-J")) {
            $_JAVA_ARGS  += "${arg:2}"
            $_SCALA_ARGS += "$arg"
            $params = $params[1..$params.Length]
        } elseif ($arg -eq "-classpath" -or $arg -eq "-cp") {
            $_SCALA_CLASSPATH = $params[1]
            $params = $params[2..$params.Length]
        } else {
            $_SCALA_ARGS += "$arg"
            $params = $params[1..$params.Length]
        }
    }

    return @{
        JAVA_ARGS       = $_JAVA_ARGS
        SCALA_ARGS      = $_SCALA_ARGS
        SCALA_CLASSPATH = $_SCALA_CLASSPATH
    }

}

function Get-ScalaLoadCompilerClasspath {
    param ( [string] $SCALA_CLASSPATH )
    $__TOOLCHAIN = @()
    $__TOOLCHAIN += Join-Path $_LIB_DIR "scala.jar"
    $__TOOLCHAIN += Join-Path $_LIB_DIR "with_compiler.jar"
    if ($SCALA_CLASSPATH) { $__TOOLCHAIN += "$SCALA_CLASSPATH" }
    return $__TOOLCHAIN -join $_PSEP
}


###################################################################################################
############################################## SCRIPT #############################################
###################################################################################################

# Parse the arguments
$_PARSE_RESULT = Get-ScalaArgumentParsing $args

# Build the java arguments
$command = @()
$command += $_PARSE_RESULT.JAVA_ARGS
$command += "-classpath"
$command += "$(Get-ScalaLoadCompilerClasspath $_PARSE_RESULT.SCALA_CLASSPATH)"
$command += "-Dscala.expandjavacp=true"
$command += "-Dscala.usejavacp=true"
$command += "-Dscala.home=$_PROG_HOME"
$command += "dotty.tools.MainGenericCompiler"
$command += $_PARSE_RESULT.SCALA_ARGS

$_JAVA_PROCESS = Start-Process -FilePath $_JAVACMD -ArgumentList $($command -join ' ') -NoNewWindow -PassThru -Wait

if ($_JAVA_PROCESS.ExitCode -ne 0) { exit 1 }

exit 0
