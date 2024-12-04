$ErrorActionPreference = 'Stop';

$unzipLocation = Split-Path -Parent $MyInvocation.MyCommand.Definition        # Get the root of chocolatey folder
$unzipLocation = Join-Path $unzipLocation "$($env:chocolateyPackageName)"     # Append the package's name
$unzipLocation = Join-Path $unzipLocation "$($env:chocolateyPackageVersion)"  # Append the package's version

# Find the path to the bin directory to create the shims
if($env:DOTTY_CI_INSTALLATION) {
    $scalaBinPath = Join-Path $unzipLocation 'bin' # Update this path if the structure inside the ZIP changes
  } else {
    $extractedDir = Get-ChildItem -Path $unzipLocation | Where-Object { $_.PSIsContainer } | Select-Object -First 1
    $scalaBinPath = Join-Path $unzipLocation $extractedDir | Join-Path -ChildPath 'bin'
  }

# Iterate through the .bat files in the bin directory and remove shims
Write-Host "Removing shims for .bat file from $scalaBinPath"
Get-ChildItem -Path $scalaBinPath -Filter '*.bat' | ForEach-Object {
    $file = $_.FullName
    Write-Host "Removing shim for $file..."
    Uninstall-BinFile -Name $_.BaseName -Path $file
}
