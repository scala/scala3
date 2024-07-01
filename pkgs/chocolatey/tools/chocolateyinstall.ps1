$ErrorActionPreference = 'Stop';
$packageArgs = @{
  packageName   = 'scala'
  fileType      = 'msi'
  url64bit      = '@LAUNCHER_URL@'

  softwareName  = 'Scala'

  silentArgs    = "/qn /norestart"
  validExitCodes= @(0)
}

Install-ChocolateyPackage @packageArgs