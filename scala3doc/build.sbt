val flexmarkVersion = "0.42.12"
val jacksonVersion = "2.9.8"
val scalaTagsVersion = "0.9.1"
val dokkaSiteVersion = "0.1.9"


libraryDependencies ++= Seq(
  "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,

  "com.virtuslab.dokka" % "dokka-site" % dokkaSiteVersion,
  "com.vladsch.flexmark" % "flexmark-all" % flexmarkVersion,
  "nl.big-o" % "liqp" % "0.6.7",
  "args4j" % "args4j" % "2.33",
)

val generateSelfDocumentation = inputKey[Unit]("Generate example documentation")
generateSelfDocumentation := Def.inputTaskDyn {
  runSelfDocumentation(classDirectory.in(Compile).value.getAbsolutePath)
}.evaluated

def runSelfDocumentation(classDir: String) = Def.taskDyn {
    val mapping = "src/main/scala=https://github.com/lampepfl/scala3doc/tree/master/src/main/scala#L"
    run.in(Compile).toTask(s" -o output/self -t $classDir -d documentation -n scala3doc -s $mapping")
}

// Uncomment to debug dokka processing (require to run debug in listen mode on 5005 port)
// javaOptions.in(run) += "-agentlib:jdwp=transport=dt_socket,server=n,address=localhost:5005,suspend=y"

fork.in(run) := true
Compile / mainClass := Some("dotty.dokka.Main")



def generateDottyDocsFromClasspath(artifacts: Seq[String]) =  Def.taskDyn {
  val roots = artifacts.mkString(java.io.File.pathSeparator)
  val mapping = "=https://github.com/lampepfl/dotty/tree/master#L"
  if (artifacts.isEmpty) Def.task {
    streams.value.log.error("Dotty lib wasn't found")
  } else Def.task {
    run.in(Compile).toTask(s" -o output/stdLib -t $roots -d dotty-docs/docs -n dotty-lib -s $mapping ").value
  } 
}

val generateScala3Documentation = taskKey[Unit]("Generate documentation for dotty lib")
generateScala3Documentation := Def.taskDyn { 
  val dotttyJars = Build.aritfactsForScala3Documentation.value.map(_.toString)

  streams.value.log.info(s"Documenting classes from:\n${dotttyJars.mkString("\n")}")

  generateDottyDocsFromClasspath(dotttyJars) 
}.value

val prepareExampleProject = taskKey[Unit]("Prepare example projet for interaction test")
prepareExampleProject := {
  


}