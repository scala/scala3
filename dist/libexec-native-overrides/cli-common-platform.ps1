function Get-ScalaCLIBinaryName {
    if ($IsWindows) {
        return 'scala-cli.exe'
    } else {
        return 'scala-cli'
    }
  }

$SCALA_CLI_CMD_WIN = Join-Path $_LIBEXEC_DIR $(Get-ScalaCLIBinaryName)
