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

import dotty.tools.dotc.config.Settings.Setting.value

import java.io.File

/** Main object for SBT.
  *
  * See [[this.process]].
  */
object Main extends Driver {

  /** Actual entrypoint from SBT.
    *
    * Internal SBT code for `sbt doc` locates this precise method with
    * reflection, and passes to us both `args` and `rootCtx`. "Internal" here
    * means that it's painful to modify this code with a plugin.
    *
    * `args` contains arguments both for us and for the compiler (see code on
    * how they're split).
    */
  override def process(args: Array[String], rootCtx: Context): Reporter = {
    // split args into ours and Dotty's
    val (dokkaStrArgs, compilerArgs) = {
      args.partitionMap { arg =>
        // our options start with this magic prefix, inserted by the SBT plugin
        val magicPrefix = "--+DOC+"
        if arg startsWith magicPrefix then
          Left(arg stripPrefix magicPrefix)
        else
          Right(arg)
      }
    }

    val (filesToCompile, ctx) = setup(compilerArgs, rootCtx)
    given Context = ctx

    // parse Dokka args
    // note: all required args should be set with SBT settings,
    // to make it easier to set and override them
    val dokkaArgs = {
      val dokkaRawArgs = new RawArgs
      val requiredArgs = Seq(
        "--tastyRoots", "", // hack, value is not used in SBT but required in CLI
        // we extract some settings from Dotty options since that's how SBT passes them
        "--name", ctx.settings.projectName.value,
        "--projectTitle", ctx.settings.projectName.value,
        "--dest", ctx.settings.outputDir.value.toString,
      )

      val allArgs = requiredArgs ++ dokkaStrArgs
      println(s"Running scala3doc with arguments: $allArgs")
      val parser = org.kohsuke.args4j.CmdLineParser(dokkaRawArgs)
      try {
        parser.parseArgument(allArgs : _*)
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
