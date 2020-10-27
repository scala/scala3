val flexmarkVersion = "0.42.12"
val jacksonVersion = "2.9.8"
val scalaTagsVersion = "0.9.1"
val dokkaSiteVersion = "0.1.9"
val dokkaVersion = "1.4.10.2"

libraryDependencies ++= Seq(
  "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,

  "com.virtuslab.dokka" % "dokka-site" % dokkaSiteVersion,
  "com.vladsch.flexmark" % "flexmark-all" % flexmarkVersion,
  "nl.big-o" % "liqp" % "0.6.7",
  "args4j" % "args4j" % "2.33",

  "org.jetbrains.dokka" % "dokka-test-api" % dokkaVersion % "test",
  "com.novocode" % "junit-interface" % "0.11" % "test",
)

val generateSelfDocumentation = inputKey[Unit]("Generate example documentation")
generateSelfDocumentation := Def.inputTaskDyn {
  generateDocumentation(classDirectory.in(Compile).value.getAbsolutePath, "scala3doc", "self", "-d documentation")
}.evaluated


def generateDocumentation(targets: String, name: String, outDir: String, params: String = "") = Def.taskDyn {
    val sourceMapping = "=https://github.com/lampepfl/dotty/tree/master#L"
    run.in(Compile).toTask(s""" -o output/$outDir -t $targets -n "$name" -s $sourceMapping $params""")
}

// Uncomment to debug dokka processing (require to run debug in listen mode on 5005 port)
// javaOptions.in(run) += "-agentlib:jdwp=transport=dt_socket,server=n,address=localhost:5005,suspend=y"

fork.in(run) := true
Compile / mainClass := Some("dotty.dokka.Main")

// There is a bug in dokka that prevents parallel tests withing the same jvm
fork.in(test) := true
Test / parallelExecution := false

val generateScala3Documentation = taskKey[Unit]("Generate documentation for dotty lib")
generateScala3Documentation := Def.taskDyn {
  val dottyJars = Build.artifactsForScala3Documentation.value.map(_.toString)
  val roots = dottyJars.mkString(java.io.File.pathSeparator)

  if (dottyJars.isEmpty) Def.task { streams.value.log.error("Dotty lib wasn't found") } 
  else generateDocumentation(roots, "Scala 3", "stdLib", "-d dotty-docs/docs")
}.value

val generateTestcasesDocumentation  = taskKey[Unit]("Generate documentation for testcases, usefull for debugging tests")
generateTestcasesDocumentation := Def.taskDyn {
  val dottyJars = Build.artifactsForScala3Documentation.value.map(_.toString)
  generateDocumentation(Build.testcasesOutputDir.in(Test).value, "Scala3doc testcases", "testcases")
}.value


buildInfoKeys in Test := Seq[BuildInfoKey](
  Build.testcasesOutputDir.in(Test),
  Build.testcasesSourceRoot.in(Test),
)

buildInfoPackage in Test := "dotty.dokka"
BuildInfoPlugin.buildInfoScopedSettings(Test)
BuildInfoPlugin.buildInfoDefaultSettings