package dotty.tools
package dottydoc

import dotty.dokka.{Args, DocConfiguration, DottyDokkaConfig, Scala3Args}

import org.jetbrains.dokka._
import org.jetbrains.dokka.utilities._
import org.jetbrains.dokka.plugability._

import dotc.core.Contexts._
import dotc.reporting.Reporter
import dotc.{ Compiler, Driver }
import dotc.config._
import dotty.tools.dotc.report.error

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
    given Context = rootCtx

    val argDefinition = new Scala3Args(error(_))
    val parsedArgs = argDefinition.extract(args.toList)

    dotty.dokka.Main.main(parsedArgs)


    rootCtx.reporter
  }

}
