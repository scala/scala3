package dotty.tools.dotc.consumetasty

import dotty.tools.dotc
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.quoted.QuoteDriver

import scala.tasty.file.TastyConsumer

object ConsumeTasty {
  def apply(classpath: String, classes: List[String], tastyConsumer: TastyConsumer): Unit = {
    if (classes.isEmpty)
      throw new IllegalArgumentException("Parameter classes should no be empty")

    class Consume extends dotc.Driver {
      override protected def newCompiler(implicit ctx: Context): dotc.Compiler =
        new TastyFromClass(tastyConsumer)
    }

    val currentClasspath = QuoteDriver.currentClasspath(getClass.getClassLoader)
    import java.io.File.{ pathSeparator => sep }
    val args = "-from-tasty" :: "-Yretain-trees" :: "-classpath" :: s"$classpath$sep$currentClasspath" :: classes
    (new Consume).process(args.toArray)
  }
}
