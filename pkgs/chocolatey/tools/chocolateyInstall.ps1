$ErrorActionPreference = 'Stop';

$unzipLocation = Split-Path -Parent $MyInvocation.MyCommand.Definition        # Get the root of chocolatey folder
$unzipLocation = Join-Path $unzipLocation "$($env:chocolateyPackageName)"     # Append the package's name
$unzipLocation = Join-Path $unzipLocation "$($env:chocolateyPackageVersion)"  # Append the package's version

# Configure the installation arguments
$packageArgs = @{
  packageName   = 'scala'
  Url64         = '@LAUNCHER_URL@'
  UnzipLocation = $unzipLocation
}

## In case we are running in the CI, add the authorisation header to fetch the zip
## See: https://docs.github.com/en/rest/actions/artifacts?apiVersion=2022-11-28#download-an-artifact
if ($env:DOTTY_CI_INSTALLATION) {
  Write-Host "Installing the Chocolatey package in Scala 3's CI" 
  $packageArgs += @{ 
    Options = @{
      Headers = @{
        Accept        = 'application/vnd.github+json'
        Authorization = "Bearer $env:DOTTY_CI_INSTALLATION"
      } 
    }
  }
}

Install-ChocolateyZipPackage @packageArgs

# Find the path to the bin directory to create the shims
if($env:DOTTY_CI_INSTALLATION) {
  $scalaBinPath = Join-Path $unzipLocation 'bin' # Update this path if the structure inside the ZIP changes
} else {
  $extractedDir = Get-ChildItem -Path $unzipLocation | Where-Object { $_.PSIsContainer } | Select-Object -First 1
  $scalaBinPath = Join-Path $unzipLocation $extractedDir | Join-Path -ChildPath 'bin'
}

# Iterate through the .bat files in the bin directory and create shims
Write-Host "Creating shims for .bat file from $scalaBinPath"
Get-ChildItem -Path $scalaBinPath -Filter '*.bat' | ForEach-Object {
    $file = $_.FullName
    Write-Host "Creating shim for $file..."
    Install-BinFile -Name $_.BaseName -Path $file
}
