package dotty.dokka

import java.util.ServiceLoader
import java.io.File
import java.util.jar._
import collection.JavaConverters._
import collection.immutable.ArraySeq

import java.nio.file.Files

import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.config.CommonScalaSettings
import dotty.tools.dotc.core.Contexts._

/** Main class for the doctool.
  *
  * The `main` method is mostly responsible just for parsing arguments and
  * configuring Dokka. After that, we hand control to Dokka.
  *
  * Other important classes:
  *
  * - [](package.DottyDokkaPlugin) is our class that Dokka calls back and which
  *   actually generates the documentation.
  * - [](package.DocContext) is our config for Dokka
  */
object Main:
  def main(args: Array[String]): Unit =
    try
      // We should create our own context here...
      val reporter = Scala3doc.run(args, (new ContextBase).initialCtx)
      // Sometimes jvm is hanging, so we want to be sure that we force shout down the jvm
      sys.exit(if reporter.hasErrors then 1 else 0)
    catch
      case a: Exception =>
        a.printStackTrace()
        // Sometimes jvm is hanging, so we want to be sure that we force shout down the jvm
        sys.exit(1)


