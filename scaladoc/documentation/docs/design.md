# Design of scaladoc

**BEWARE this is not complete documentation yet but rather a draft mainly to test linking from static site to code**

## Interface

scaladoc is intednted to be use with sbt as well as from commend line or from other buildtools. The main entry point to processing is [Main](dotty.tools.scaladoc.Main$) class with [[dotty.tools.scaladoc.Main$.main]].