import sbt._
import sbt.io.IO

import sbt.dsl.LinterLevel.Ignore

lazy val compileSrcTree = taskKey[Unit]("Example project")

compileSrcTree := {
  val log         = streams.value.log
  val baseDir     = baseDirectory.value
  val srcTreeDir  = baseDir / "local" / "project"
  val outDir      = baseDir / "local" / "out"

  IO.delete(outDir)
  IO.createDirectory(outDir)                              // mkdir -p

  val sources: Seq[String] =
    (srcTreeDir ** "*.scala").get.map(_.getPath)          // find  all .scala

  if (sources.isEmpty)
    streams.value.log.warn(s"No .scala files found under $srcTreeDir")
  else {
    val cmd = ("scalac" +: "-d" +: outDir.getPath +: sources).mkString(" ")
    Command.process(cmd, state.value)
  }
}

lazy val ensureApiDir = taskKey[Unit]("Create <repo>/local/api if it’s missing")

ensureApiDir := {
  val dir = (ThisBuild / baseDirectory).value / "local" / "api"
  IO.createDirectory(dir)
}

addCommandAlias(
  "myrefresh",
  ";compileSrcTree; ensureApiDir ; scaladoc/runMain dotty.tools.scaladoc.Main -siteroot /dev/null -project Foo -project-version 0.0.1 -d local/api local/out"
)

addCommandAlias(
  "myscaladoc",
  "; ensureApiDir ; scaladoc/runMain dotty.tools.scaladoc.Main -siteroot /dev/null -project Foo -project-version 0.0.1 -d local/api local/out"
)