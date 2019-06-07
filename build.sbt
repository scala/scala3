val dotty = Build.dotty
val `dotty-bootstrapped` = Build.`dotty-bootstrapped`
val `dotty-interfaces` = Build.`dotty-interfaces`
val `dotty-doc` = Build.`dotty-doc`
val `dotty-doc-bootstrapped` = Build.`dotty-doc-bootstrapped`
val `dotty-compiler` = Build.`dotty-compiler`
val `dotty-compiler-bootstrapped` = Build.`dotty-compiler-bootstrapped`
val `dotty-library` = Build.`dotty-library`
val `dotty-library-bootstrapped` = Build.`dotty-library-bootstrapped`
val `dotty-library-bootstrappedJS` = Build.`dotty-library-bootstrappedJS`
val `dotty-sbt-bridge` = Build.`dotty-sbt-bridge`
val `dotty-sbt-bridge-tests` = Build.`dotty-sbt-bridge-tests`
val `dotty-language-server` = Build.`dotty-language-server`
val `dotty-bench` = Build.`dotty-bench`
val `dotty-bench-bootstrapped` = Build.`dotty-bench-bootstrapped`
val `dotty-semanticdb` = Build.`dotty-semanticdb`
val `dotty-semanticdb-input` = Build.`dotty-semanticdb-input`
val `scala-library` = Build.`scala-library`
val `scala-compiler` = Build.`scala-compiler`
val `scala-reflect` = Build.`scala-reflect`
val scalap = Build.scalap
val dist = Build.dist
val `dist-bootstrapped` = Build.`dist-bootstrapped`
val `community-build` = Build.`community-build`

val sjsSandbox = Build.sjsSandbox
val sjsJUnitTests = Build.sjsJUnitTests

val `sbt-dotty` = Build.`sbt-dotty`
val `vscode-dotty` = Build.`vscode-dotty`

inThisBuild(Build.thisBuildSettings)
inScope(Global)(Build.globalSettings)
