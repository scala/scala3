package dotty.tools.scaladoc

import java.util.ServiceLoader
import java.io.File
import java.util.jar._
import collection.JavaConverters._
import collection.immutable.ArraySeq

import java.nio.file.Files

import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.config.CommonScalaSettings
import dotty.tools.dotc.core.Contexts._

/** Main class for the doctool when used from cli. */
object Main:
  def main(args: Array[String]): Unit =
    try
      // We should create our own context here...
      val reporter = Scaladoc.run(args, (new ContextBase).initialCtx)
      // Sometimes jvm is hanging, so we want to be sure that we force shout down the jvm
      sys.exit(if reporter.hasErrors then 1 else 0)
    catch
      case a: Exception =>
        a.printStackTrace()
        // Sometimes jvm is hanging, so we want to be sure that we force shout down the jvm
        sys.exit(1)


