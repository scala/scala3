package scala.tasty.file

import java.net.URLClassLoader

import dotty.tools.dotc
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.util.ClasspathFromClassloader

import java.nio.file.Paths
import java.io.File
import java.io.File.{ pathSeparator => sep }
import scala.annotation.tailrec

import scala.tasty.file.TastyConsumer

// TODO: Move this logic into TastyConumer to simplify the API
object ConsumeTasty {

  /** Load and process TASTy files using TASTy reflect
   *
   * @param classpath Classpath where the classes are located
   * @param classes classes to be consumed
   * @param tastyConsumer consumer that will process the tasty trees
   */
  def apply(classpath: String, classes: List[String], tastyConsumer: TastyConsumer): Unit = {
    if (classes.isEmpty)
      throw new IllegalArgumentException("Parameter classes should no be empty")

    class Consume extends dotc.Driver {
      override protected def newCompiler(implicit ctx: Context): dotc.Compiler =
        new TastyFromClass(tastyConsumer)
    }

    val currentClasspath = ClasspathFromClassloader(getClass.getClassLoader)
    val args = "-from-tasty" :: "-Yretain-trees" :: "-classpath" :: s"$classpath$sep$currentClasspath" :: classes
    (new Consume).process(args.toArray)
  }

}
