<h1 align=center>Configuration for Chocolatey</h1>

Official support for Chocolatey started by the release of Scala 3.6.0

> [!IMPORTANT]
> This folder contains the templates to generate the configuration for Chocolatey.
> The `scala.nuspec` and `chocolateyInstall.ps1` files needs to be rewritten by changing the following placeholders:
> - @LAUNCHER_VERSION@ : Placeholder for the current scala version to deploy
> - @LAUNCHER_URL@     : Placeholder for the URL to the windows zip released on GitHub
> - @LAUNCHER_SHA256@  : Placeholder for the SHA256 of the msi file released on GitHub

## Important information

- How to create a *Chocolatey* package: https://docs.chocolatey.org/en-us/create/create-packages/
- Guidelines to follow for the package icon: https://docs.chocolatey.org/en-us/create/create-packages/#package-icon-guidelines
- `.nuspec` format specification: https://learn.microsoft.com/en-gb/nuget/reference/nuspec
