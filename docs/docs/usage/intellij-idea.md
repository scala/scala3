---
layout: doc-page
title: Building Dotty with Intellij IDEA
---

Dotty compiler support is available in the [Scala plugin nightly] starting from 2.2.39. 
You need to install [IDEA 2016.1] to try it.

## To create a new project with Dotty

1. Open New Project dialog and select `Scala` > `Dotty`
2. Proceed as usual and don't forget to create or select Dotty SDK.

## To compile an existing Scala project with Dotty

1. Create a new Dotty SDK:
   `Project Structure` > `Global libraries` > `New Global Library` > `Dotty SDK`
2. Replace Scala SDK with Dotty SDK in:
   `Project Structure` > `Modules` > `Dependencies`

Java 1.8 should be used as the Project/Module SDK. You also need to enable the
Scala Compile Server to use Dotty compiler.

## Notes
* Dotty support is experimental, many features including code highlighting and
  worksheet are not ready yet.
* You can download the latest version of Dotty without creating a new Dotty SDK
  with the `Update snapshot` button in the Dotty SDK library settings.
* Please report any problems to the [IntelliJ Scala issue tracker] or write
  to the [IntelliJ Scala gitter]

[Scala plugin nightly]: https://confluence.jetbrains.com/display/SCA/Scala+Plugin+Nightly
[IDEA 2016.1]: https://www.jetbrains.com/idea/nextversion/
[IntelliJ Scala issue tracker]: https://youtrack.jetbrains.com/issues/SCL
[IntelliJ Scala gitter]: https://gitter.im/JetBrains/intellij-scala
