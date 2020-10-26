val flexmarkVersion = "0.42.12"
val jacksonVersion = "2.9.8"
val scalaTagsVersion = "0.9.1"
val dokkaSiteVersion = "0.1.9"


libraryDependencies ++= Seq(
  "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,


  "com.virtuslab.dokka" % "dokka-site" % dokkaSiteVersion,
  "org.scala-sbt" % "io_2.13" % "1.3.4",
  "com.vladsch.flexmark" % "flexmark-all" % flexmarkVersion,
  "com.lihaoyi" % "scalatags_2.13" % scalaTagsVersion,
  "nl.big-o" % "liqp" % "0.6.7",
  "args4j" % "args4j" % "2.33",
) //a a

val generateSelfDocumentation = inputKey[Unit]("Generate example documentation")
generateSelfDocumentation := {
  run.in(Compile).fullInput(" -o output/self -t target/scala-0.27/classes -d documentation -n scala3doc -s src/main/scala=https://github.com/lampepfl/scala3doc/tree/master/src/main/scala#L").evaluated // TODO #35 proper sbt integration
}


// Uncomment to debug dokka processing (require to run debug in listen mode on 5005 port)
// javaOptions.in(run) += "-agentlib:jdwp=transport=dt_socket,server=n,address=localhost:5005,suspend=y"

fork.in(run) := true
Compile / mainClass := Some("dotty.dokka.Main")


// TODO #35 proper sbt integration
val generateDottyLibDocumentation = taskKey[Unit]("Generate documentation for dotty lib")
generateDottyLibDocumentation :=  Def.taskDyn {
  val dotttyLib = fullClasspath.in(Compile).value.find{ a =>
    val info = a.get(moduleID.key)
    info.nonEmpty &&
     info.get.organization == "ch.epfl.lamp" &&
     info.get.name.startsWith("dotty-library")
  }
  if (dotttyLib.isEmpty) Def.task {
    streams.value.log.error("Dotty lib wasn't found")
  } else Def.task {
    run.in(Compile).toTask(s" -o output/stdLib -t ${dotttyLib.get.data} -d dotty-docs/docs -n dotty-lib -s library/src=https://github.com/lampepfl/dotty/tree/master/library/src#L").value
  } 
}.value

