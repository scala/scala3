package dotty.tools
package dotc
package fromtasty

import scala.util.control.NonFatal

import dotty.tools.io.Path

import java.nio.file.{Files, Paths}

object Debug {
  def main(args: Array[String]): Unit = {
    // Preload scala.util.control.NonFatal. Otherwise, when trying to catch a StackOverflowError,
    // we may try to load it but fail with another StackOverflowError and lose the original exception,
    // see <https://groups.google.com/forum/#!topic/scala-user/kte6nak-zPM>.
    val _ = NonFatal


    println("From tasty debug driver")
    assert(!args.contains("-d"))

    val fromSourcesOut = Files.createTempDirectory(Paths.get("out").toAbsolutePath, "from-sources-tmp")

    println(s"Compiling scala to sources to $fromSourcesOut")
    val compilation1 = dotc.Main.process("-d" +: fromSourcesOut.toString +: args)

    if (compilation1.hasErrors) {
      println("Failed compilation from sources")
      sys.exit(1)
    }

    val fromTastyOut = Files.createTempDirectory(Paths.get("out").toAbsolutePath, "from-tasty-tmp")

    val ext = "hasTasty"
    val classes = Path(fromSourcesOut).walk.filter(x => x.isFile && x.extension == ext).map { x =>
      val source = x.toString
      source.substring(fromSourcesOut.toString.length + 1, source.length - ext.length - 1).replace('/', '.')
    }.toList

    val fromTastyArgs =
      "-from-tasty" :: insertClasspathInArgs(args.filterNot(_.endsWith(".scala")).toList, fromSourcesOut.toString) ::: classes

    println(s"Compiling TASTY to sources from $fromSourcesOut to $fromTastyOut")
    val compilation2 = dotc.Main.process(fromTastyArgs.toArray)

    if (compilation2.hasErrors) {
      println("Failed compilation from sources")
      sys.exit(1)
    }

    Path(fromSourcesOut).deleteRecursively()
    Path(fromTastyOut).deleteRecursively()
  }

  private def insertClasspathInArgs(args: List[String], cp: String): List[String] = {
    val (beforeCp, fromCp) = args.span(_ != "-classpath")
    val classpath = fromCp.drop(1).headOption.fold(cp)(_ + ":" + cp)
    "-classpath" :: classpath :: beforeCp ::: fromCp.drop(2)
  }
}
