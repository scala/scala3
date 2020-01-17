package dotty.tools.dotc.consumetasty

import java.net.URLClassLoader

import dotty.tools.dotc
import dotty.tools.dotc.core.Contexts._

import java.nio.file.Paths
import java.io.File
import java.io.File.{ pathSeparator => sep }
import scala.annotation.tailrec

import scala.tasty.file.TastyConsumer

object ConsumeTasty {

  def apply(classpath: String, classes: List[String], tastyConsumer: TastyConsumer): Unit = {
    if (classes.isEmpty)
      throw new IllegalArgumentException("Parameter classes should no be empty")

    class Consume extends dotc.Driver {
      override protected def newCompiler(implicit ctx: Context): dotc.Compiler =
        new TastyFromClass(tastyConsumer)
    }

    val currentClasspath = classpathFromClassloader(getClass.getClassLoader)
    val args = "-from-tasty" :: "-Yretain-trees" :: "-classpath" :: s"$classpath$sep$currentClasspath" :: classes
    (new Consume).process(args.toArray)
  }

  /** Attempt to recreate a classpath from a classloader.
   *
   *  BEWARE: with exotic enough classloaders, this may not work at all or do
   *  the wrong thing.
   */
   private def classpathFromClassloader(cl: ClassLoader): String = {
    @tailrec
    def loop(cl: ClassLoader, suffixClasspath: String): String =
      cl match {
        case cl: URLClassLoader =>
          val updatedClasspath = cl.getURLs
            .map(url => Paths.get(url.toURI).toAbsolutePath.toString)
            .mkString(
              "",
              File.pathSeparator,
              if (suffixClasspath.isEmpty) "" else File.pathSeparator + suffixClasspath
            )
          loop(cl.getParent, updatedClasspath)
        case _ =>
          suffixClasspath
      }

    loop(cl, "")
  }
}