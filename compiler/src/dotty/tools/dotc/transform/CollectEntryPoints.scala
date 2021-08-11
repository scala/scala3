package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.transform.MegaPhase._
import java.io.{File => _}

import dotty.tools.dotc.core._
import SymDenotations._
import Contexts._
import Types._
import Symbols._
import Phases._
import dotty.tools.dotc.util.SourcePosition
import Decorators._
import StdNames.nme
import dotty.tools.io.JarArchive
import dotty.tools.backend.jvm.GenBCode

/**
 * Small phase to be run to collect main classes and store them in the context.
 * The general rule to run this phase is either:
 * - The output of compilation is JarArchive and there is no `-Xmain-class` defined
 * - The compiler is run from sbt and is forced by flags forcing `ExtractorAPI`
 *
 * The following flags affect this phase:
 *   -d path.jar
 *   -Xmain-class
 *   -Yforce-sbt-phases
 *   -Ydump-sbt-inc
 */
class CollectEntryPoints extends MiniPhase:
  def phaseName: String = "Collect entry points"

  override def isRunnable(using Context): Boolean =
    def forceRun = (ctx.settings.XmainClass.isDefault && ctx.settings.outputDir.value.isInstanceOf[JarArchive]) ||
                    ctx.settings.YdumpSbtInc.value ||
                    ctx.settings.YforceSbtPhases.value
    super.isRunnable && (ctx.sbtCallback != null || forceRun)


  override def transformTypeDef(tree: tpd.TypeDef)(using Context): tpd.Tree =
    getEntryPoint(tree).map(registerEntryPoint)
    tree

  private def getEntryPoint(tree: tpd.TypeDef)(using Context): Option[String] =
    val sym = tree.symbol
    import dotty.tools.dotc.core.NameOps.stripModuleClassSuffix
    val name = sym.fullName.stripModuleClassSuffix.toString
    Option.when(sym.isStatic && !sym.is(Flags.Trait) && ctx.platform.hasMainMethod(sym))(name)

  private def registerEntryPoint(s: String)(using Context) = {
    genBCodePhase match {
      case genBCodePhase: GenBCode =>
        genBCodePhase.registerEntryPoint(s)
      case _ =>
    }
  }
