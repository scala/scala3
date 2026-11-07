package dotty.tools
package dotc
package fromtasty

import dotty.tools.dotc.classpath.ClassPath
import dotty.tools.nio.*

object Debug {
  def main(args: Array[String]): Unit = {
    assert(!args.contains("-d"))

    val outPath = FileContainer.getOrCreateOnDisk("out")

    val tmpOut = FileContainer.createTemporaryOnDisk("from-tasty-tmp")

    val fromSourcesOut = tmpOut.getOrCreateContainer("from-source")

    println("Compiling from .scala sources")
    val compilation1 = dotc.Main.process("-d" +: fromSourcesOut.toString +: args)

    if (compilation1.hasErrors) {
      println("Failed compilation from sources")
      tmpOut.deleteRecursively()
      sys.exit(1)
    }

    val fromTastyOut = tmpOut.getOrCreateContainer("from-tasty")

    val tastyFiles =
      fromSourcesOut.recursiveEntries
        .collect { case f: File if f.extension.isTasty => f.path }
        .toList

    val fromTastyArgs =
      "-from-tasty" ::
      "-d" :: fromTastyOut.path ::
      insertClasspathInArgs(args.filterNot(_.endsWith(".scala")).toList, fromSourcesOut.path) :::
      tastyFiles

    println("Compiling from .tasty sources")
    val compilation2 = dotc.Main.process(fromTastyArgs.toArray)

    if (compilation2.hasErrors) {
      println("Failed compilation from TASTY")
      println("Compilation input: " + fromSourcesOut.path)
      // In this case we do not delete the generated class files to allow further debugging.
      // For example `dotc -decompile` on one of the intermediate class files.
      sys.exit(1)
    } else {
      println("Recompilation successful")
    }

    tmpOut.deleteRecursively()
  }

  private def insertClasspathInArgs(args: List[String], cp: String): List[String] = {
    val (beforeCp, fromCp) = args.span(_ != "-classpath")
    val classpath = fromCp.drop(1).headOption.fold(cp)(_ + ClassPath.pathSeparator + cp)
    "-classpath" :: classpath :: beforeCp ::: fromCp.drop(2)
  }
}
