# Dotty dokka

This project is PoC to test if [dokka](https://github.com/Kotlin/dokka) can be used as forntend to generate documentation for Scala 3 based projects using TASTY as a source of information about types etc.

## Scope of work

In order to complete this PoC we should have a version that can:

1) parse scaladoc format and render it properly (for supported entries, see below)
2) render documentation for packages, classes and methods
3) render multiple parameters list correctly
4) render correctly types however we allow to use text for types that cannot be rednered using Kotlin/Java
5) Properly link to classes and methods from signatures and from within scaladocs

Nice to have:

1) Render type members
2) ...

