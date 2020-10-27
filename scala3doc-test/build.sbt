val dokkaVersion = "1.4.10.2"

libraryDependencies ++= Seq(
  "org.jetbrains.dokka" % "dokka-test-api" % dokkaVersion % "test", // TODO move testing utils to dokka-site
  "com.novocode" % "junit-interface" % "0.11" % "test",
)

val testOutputDir = taskKey[String]("Root directory where tests classses are generated")
testOutputDir := (Compile/target/classDirectory).value.getAbsolutePath.toString

val testSourceRoot = taskKey[String]("Root directory where tests sources are generated")
testSourceRoot := (baseDirectory.value / "src").getAbsolutePath.toString


buildInfoKeys in Test := Seq[BuildInfoKey](
  testOutputDir,
  testSourceRoot,
)
buildInfoPackage in Test := "dotty.dokka"
BuildInfoPlugin.buildInfoScopedSettings(Test)
BuildInfoPlugin.buildInfoDefaultSettings

// Uncomment to debug dokka processing (require to run debug in listen mode on 5005 port)
// javaOptions.in(run) += "-agentlib:jdwp=transport=dt_socket,server=n,address=localhost:5005,suspend=y"

fork.in(run) := true

// There is a bug in dokka that prevents parallel tests withing the same jvm
fork.in(test) := true
Test / parallelExecution := false
