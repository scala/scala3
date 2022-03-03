package dotty.tools.dotc
package transform

import core._
import ast.tpd
import MegaPhase._
import Contexts._
import Symbols._
import Phases._
import dotty.tools.io.JarArchive
import dotty.tools.backend.jvm.GenBCode

/**
 * Small phase to be run to collect main classes and store them in the context.
 * The general rule to run this phase is:
 * - The output of compilation is JarArchive
 * - There is no `-Xmain-class` defined
 *
 * The following flags affect this phase:
 *   -d path.jar
 *   -Xmain-class
 */
class CollectEntryPoints extends MiniPhase:

  override def phaseName: String = CollectEntryPoints.name

  override def description: String = CollectEntryPoints.description

  override def isRunnable(using Context): Boolean =
    def forceRun = ctx.settings.XmainClass.isDefault && ctx.settings.outputDir.value.isInstanceOf[JarArchive]
    super.isRunnable && forceRun

  override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree =
    getEntryPoint(tree).map(registerEntryPoint)
    tree

  private def getEntryPoint(tree: tpd.TypeDef)(using Context): Option[String] =
    val sym = tree.symbol
    import core.NameOps.stripModuleClassSuffix
    val name = sym.fullName.stripModuleClassSuffix.toString
    Option.when(sym.isStatic && !sym.is(Flags.Trait) && ctx.platform.hasMainMethod(sym))(name)

  private def registerEntryPoint(s: String)(using Context) = {
    genBCodePhase match {
      case genBCodePhase: GenBCode =>
        genBCodePhase.registerEntryPoint(s)
      case _ =>
    }
  }

object CollectEntryPoints:
  val name: String = "Collect entry points"
  val description: String = "collect all entry points and save them in the context"
