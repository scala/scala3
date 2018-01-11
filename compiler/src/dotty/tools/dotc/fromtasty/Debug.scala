package dotty.tools
package dotc
package fromtasty

import scala.util.control.NonFatal

import dotty.tools.io.Directory

import java.nio.file.{Files, Paths}

object Debug {
  def main(args: Array[String]): Unit = {
    // Preload scala.util.control.NonFatal. Otherwise, when trying to catch a StackOverflowError,
    // we may try to load it but fail with another StackOverflowError and lose the original exception,
    // see <https://groups.google.com/forum/#!topic/scala-user/kte6nak-zPM>.
    val _ = NonFatal

    assert(!args.contains("-d"))

    val tmpOut = Files.createTempDirectory(Paths.get("out").toAbsolutePath, "from-tasty-tmp")

    val fromSourcesOut = Files.createDirectory(tmpOut.resolve("from-source"))

    println(s"Compiling .scala")
    val compilation1 = dotc.Main.process("-d" +: fromSourcesOut.toString +: args)

    if (compilation1.hasErrors) {
      println("Failed compilation from sources")
      sys.exit(1)
    }

    val fromTastyOut = Files.createDirectory(tmpOut.resolve("from-tasty"))

    val ext = "hasTasty"
    val classes = Directory(fromSourcesOut).walk.filter(x => x.isFile && x.extension == ext).map { x =>
      val source = x.toString
      source.substring(fromSourcesOut.toString.length + 1, source.length - ext.length - 1).replace('/', '.')
    }.toList

    val fromTastyArgs = {
      "-from-tasty" ::
      "-d" :: fromTastyOut.toString ::
      insertClasspathInArgs(args.filterNot(_.endsWith(".scala")).toList, fromSourcesOut.toString) :::
      classes
    }

    println(s"Compiling TASTY")
    val compilation2 = dotc.Main.process(fromTastyArgs.toArray)

    if (compilation2.hasErrors) {
      println("Failed compilation from TASTY")
      println("Compilation input: " + fromSourcesOut)
      sys.exit(1)
    }

    Directory(tmpOut).deleteRecursively()
  }

  private def insertClasspathInArgs(args: List[String], cp: String): List[String] = {
    val (beforeCp, fromCp) = args.span(_ != "-classpath")
    val classpath = fromCp.drop(1).headOption.fold(cp)(_ + ":" + cp)
    "-classpath" :: classpath :: beforeCp ::: fromCp.drop(2)
  }
}
