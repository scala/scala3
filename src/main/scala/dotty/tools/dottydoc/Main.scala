package dotty.tools
package dottydoc

import dotty.dokka.{Args, RawArgs, DocConfiguration, DottyDokkaConfig}

import org.jetbrains.dokka._
import org.jetbrains.dokka.utilities._
import org.jetbrains.dokka.plugability._

import dotc.core.Contexts._
import dotc.reporting.Reporter
import dotc.{ Compiler, Driver }
import dotc.config._

import java.io.File

object Main extends Driver {

  override def process(args: Array[String], rootCtx: Context): Reporter = {
    // split args into ours and Dotty's
    val (dokkaStrArgs, compilerArgs) = {
      // first "--" arg marks the end of Scala3doc-specific options
      // it is inserted by SBT plugin
      val split = args.splitAt(args.indexOf("--"))
      (split._1, split._2.tail)
    }

    val (filesToCompile, ctx) = setup(compilerArgs, rootCtx)
    given as Context = ctx

    // parse Dokka args
    // note: all required args should be set with SBT settings,
    // to make it easier to set and override them
    val dokkaArgs = {
      val dokkaRawArgs = new RawArgs
      val requiredArgs = Seq(
        "--tastyRoots", "", // hack, value is not used in SBT but required in CLI
        // we extract some settings from Dotty options since that's how SBT passes them
        "--projectTitle", ctx.settings.projectName.value,
        "--output", ctx.settings.outputDir.value.toString,
      )

      val parser = org.kohsuke.args4j.CmdLineParser(dokkaRawArgs)
      try {
        parser.parseArgument(requiredArgs ++ dokkaStrArgs : _*)
      } catch {
        case ex: org.kohsuke.args4j.CmdLineException =>
          // compiler errors are reported in SBT
          dotc.report.error(s"Error when parsing Scala3doc options: ${ex.getMessage}")
          throw ex
      }
      dokkaRawArgs.toArgs
    }

    val config = DocConfiguration.Sbt(dokkaArgs, filesToCompile, ctx)
    val dokkaCfg = new DottyDokkaConfig(config)
    new DokkaGenerator(dokkaCfg, DokkaConsoleLogger.INSTANCE).generate()

    rootCtx.reporter
  }

}
