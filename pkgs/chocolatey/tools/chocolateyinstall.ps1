$ErrorActionPreference = 'Stop';
$packageArgs = @{
  packageName   = 'scala'
  fileType      = 'MSI'
  url64bit      = '@LAUNCHER_URL@'

  softwareName  = 'Scala' 
  checksum64    = '@LAUNCHER_SHA256@'
  checksumType64= 'sha256'

  silentArgs    = "/qn /norestart"
  validExitCodes= @(0)
}

Install-ChocolateyPackage @packageArgs