// Aggregate projects
val `scala3-nonbootstrapped` = Build.`scala3-nonbootstrapped`
val `scala3-bootstrapped` = Build.`scala3-bootstrapped`

val `scala3-interfaces` = Build.`scala3-interfaces`

// Compiler projects
val `scala3-compiler-nonbootstrapped` = Build.`scala3-compiler-nonbootstrapped`
val `scala3-compiler-bootstrapped` = Build.`scala3-compiler-bootstrapped`

val `scala3-repl` = Build.`scala3-repl`

// The Standard Library
val `scala-library-nonbootstrapped` = Build.`scala-library-nonbootstrapped`
val `scala3-library-nonbootstrapped` = Build.`scala3-library-nonbootstrapped`
val `scala-library-bootstrapped` = Build.`scala-library-bootstrapped`
val `scala3-library-bootstrapped` = Build.`scala3-library-bootstrapped`
val `scala-library-sjs` = Build.`scala-library-sjs`
val `scala3-library-sjs` = Build.`scala3-library-sjs`

val `scala3-sbt-bridge-bootstrapped` = Build.`scala3-sbt-bridge-bootstrapped`
val `scala3-sbt-bridge-nonbootstrapped` = Build.`scala3-sbt-bridge-nonbootstrapped`
val `scala3-staging` = Build.`scala3-staging`
val `scala3-tasty-inspector` = Build.`scala3-tasty-inspector`
val `scala3-language-server` = Build.`scala3-language-server`
//val `scala3-bench` = Build.`scala3-bench`
//val `scala3-bench-bootstrapped` = Build.`scala3-bench-bootstrapped`
//val `scala3-bench-micro` = Build.`scala3-bench-micro`
//val `scala3-bench-run` = Build.`scala3-bench-run`
val `tasty-core-nonbootstrapped` = Build.`tasty-core-nonbootstrapped`
val `tasty-core-bootstrapped` = Build.`tasty-core-bootstrapped`
val scaladoc = Build.scaladoc
val `scaladoc-testcases` = Build.`scaladoc-testcases`
val `scaladoc-js-common` = Build.`scaladoc-js-common`
val `scaladoc-js-main` = Build.`scaladoc-js-main`
val `scaladoc-js-contributors` = Build.`scaladoc-js-contributors`
val dist = Build.dist
val `dist-mac-x86_64` = Build.`dist-mac-x86_64`
val `dist-mac-aarch64` = Build.`dist-mac-aarch64`
val `dist-win-x86_64` = Build.`dist-win-x86_64`
val `dist-linux-x86_64` = Build.`dist-linux-x86_64`
val `dist-linux-aarch64` = Build.`dist-linux-aarch64`
val `community-build` = Build.`community-build`
val `scala3-presentation-compiler` = Build.`scala3-presentation-compiler`
val `scala3-presentation-compiler-testcases` = Build.`scala3-presentation-compiler-testcases`

val sjsSandbox = Build.sjsSandbox
val sjsJUnitTests = Build.sjsJUnitTests
val sjsCompilerTests = Build.sjsCompilerTests

inThisBuild(Build.thisBuildSettings)
inScope(Global)(Build.globalSettings)
