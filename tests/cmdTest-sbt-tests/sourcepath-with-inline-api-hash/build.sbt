import java.util.Properties

val prepareSources = taskKey[Unit]("Copy changes to the src directory")
val copyChanges = taskKey[Unit]("Copy changes to the src directory")

val srcDir = settingKey[File]("The directory to copy changes to")
val changesDir = settingKey[File]("The directory to copy changes from")

srcDir := (ThisBuild / baseDirectory).value / "src" / "main" / "scala"
changesDir := (ThisBuild / baseDirectory).value / "changes"

prepareSources := IO.copyFile(changesDir.value / "zz.original.scala", srcDir.value / "a" / "zz.scala")
copyChanges := IO.copyFile(changesDir.value / "zz.new.scala", srcDir.value / "a" / "zz.scala")

(Compile / scalacOptions) ++= Seq(
  "-sourcepath", (Compile / sourceDirectories).value.map(_.getAbsolutePath).distinct.mkString(java.io.File.pathSeparator),
)
