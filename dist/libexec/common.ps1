###################################################################################################
###                                  POWERSHELL COMMON SCRIPT                                   ###
###                                                                                             ###
### Author: Hamza REMMAL <hamza.remmal@epfl.ch>                                                 ###
### Since : Scala 3.6.0                                                                         ###
###################################################################################################


###################################################################################################
######################################## UTILITY FUNCTIONS ########################################
###################################################################################################

function Get-JavaClasspathSeparator {
  if ($IsWindows) {
      return ';'
  } else {
      return ':'
  }
}

function Get-FetchScalaVersion {
  if (Test-Path $_VERSION_FILE) {
      foreach ($line in Get-Content $_VERSION_FILE) {
          if ($line.StartsWith("version:=")) {
              return $line.Substring(9)
          }
      }
      Write-Error "Error: 'VERSION' file does not contain 'version' entry in ($_VERSION_FILE)"
  } else {
      Write-Error "Error: 'VERSION' file is missing in ($_VERSION_FILE)"
  }
}

###################################################################################################
############################################ LOAD JAVA ############################################
###################################################################################################

$javaPath = Get-Command java -ErrorAction SilentlyContinue
if ($javaPath) { $_JAVACMD = $javaPath.Source }

if (-not (Test-Path $_JAVACMD)) {
  Write-Error "Error: Java executable not found ($_JAVACMD)"
  exit 1
}

if (-not $_PROG_HOME) {
  Write-Error "Error: Variable _PROG_HOME undefined"
  exit 1
}

###################################################################################################
######################################## VARIOUS VARIABLES ########################################
###################################################################################################

$_LIB_DIR        = Join-Path $_PROG_HOME "lib"
$_LIBEXEC_DIR    = Join-Path $_PROG_HOME "libexec"
$_BIN_DIR        = Join-Path $_PROG_HOME "bin"
$_MVN_REPOSITORY = "file:///$($_PROG_HOME -replace '\\', '/')/maven2"
$_VERSION_FILE   = Join-Path $_PROG_HOME "VERSION"
$_PSEP           = Get-JavaClasspathSeparator
