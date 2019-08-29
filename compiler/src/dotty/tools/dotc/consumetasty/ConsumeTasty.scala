package dotty.tools.dotc.consumetasty

import java.net.URLClassLoader

import dotty.tools.dotc
import dotty.tools.dotc.core.Contexts._

import scala.tasty.file.TastyConsumer

object ConsumeTasty {

  def apply(classpath: String, classes: List[String], tastyConsumer: TastyConsumer): Unit = {
    if (classes.isEmpty)
      throw new IllegalArgumentException("Parameter classes should no be empty")

    class Consume extends dotc.Driver {
      override protected def newCompiler(implicit ctx: Context): dotc.Compiler =
        new TastyFromClass(tastyConsumer)
    }

    val currentClasspath = getCurrentClasspath(getClass.getClassLoader)
    import java.io.File.{ pathSeparator => sep }
    val args = "-from-tasty" :: "-Yretain-trees" :: "-classpath" :: s"$classpath$sep$currentClasspath" :: classes
    (new Consume).process(args.toArray)
  }

  private def getCurrentClasspath(cl: ClassLoader): String = {
    val classpath0 = System.getProperty("java.class.path")
    cl match {
      case cl: URLClassLoader =>
        // Loads the classes loaded by this class loader
        // When executing `run` or `test` in sbt the classpath is not in the property java.class.path
        import java.nio.file.Paths
        val newClasspath = cl.getURLs.map(url => Paths.get(url.toURI).toString)
        newClasspath.mkString("", java.io.File.pathSeparator, if (classpath0 == "") "" else java.io.File.pathSeparator + classpath0)
      case _ => classpath0
    }
  }
}

