This directory is for keeping all the compiler related docs.

For serving this files, one should notice that files under `docs/docs/reference` and `docs/docs/usage/scaladoc` are designed to be compatible with `docs.scala-lang` jekyll page.
If one wants to generate static site with these files, should firstly run the script from `project/scripts/processDocs.sc`, for example with:
`bin/scala project/scripts/processDocs.sc` and then using sbt task `sbt scaladoc/generateScalaDocumentation` generate the whole scaladoc for dotty.
