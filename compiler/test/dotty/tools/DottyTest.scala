package dotty
package tools

import vulpix.TestConfiguration

import dotc.core._
import dotc.core.Comments.{ContextDoc, ContextDocstrings}
import dotc.core.Contexts._
import dotc.core.Symbols._
import dotc.core.Flags._
import Types._, Symbols._, Decorators._
import dotc.printing.Texts._
import dotc.reporting.ConsoleReporter
import dotc.core.Decorators._
import dotc.ast.tpd
import dotc.Compiler

import dotc.core.Phases.Phase

trait DottyTest extends ContextEscapeDetection {

  dotc.parsing.Scanners // initialize keywords

  implicit var ctx: Context = initialCtx

  protected def initialCtx: FreshContext = {
    val base = new ContextBase {}
    import base.settings._
    val ctx = base.initialCtx.fresh
    initializeCtx(ctx)
    // when classpath is changed in ctx, we need to re-initialize to get the
    // correct classpath from PathResolver
    base.initialize()(ctx)
    ctx
  }

  override def getCtx: Context = ctx
  override def clearCtx() = {
    ctx = null
  }

  protected def initializeCtx(fc: FreshContext): Unit = {
    fc.setSetting(fc.settings.encoding, "UTF8")
    fc.setSetting(fc.settings.classpath, TestConfiguration.basicClasspath)
    fc.setProperty(ContextDoc, new ContextDocstrings)
  }

  private def compilerWithChecker(phase: String)(assertion: (tpd.Tree, Context) => Unit) = new Compiler {
    override def phases = {
      val allPhases = super.phases
      val targetPhase = allPhases.flatten.find(p => p.phaseName == phase).get
      val groupsBefore = allPhases.takeWhile(x => !x.contains(targetPhase))
      val lastGroup = allPhases.find(x => x.contains(targetPhase)).get.takeWhile(x => !(x eq targetPhase))
      val checker = new Phase {
        def phaseName = "assertionChecker"
        override def run(implicit ctx: Context): Unit = assertion(ctx.compilationUnit.tpdTree, ctx)
      }
      val lastGroupAppended = List(lastGroup ::: targetPhase :: Nil)

      groupsBefore ::: lastGroupAppended ::: List(List(checker))
    }
  }

  def checkCompile(checkAfterPhase: String, source: String)(assertion: (tpd.Tree, Context) => Unit): Context = {
    val c = compilerWithChecker(checkAfterPhase)(assertion)
    val run = c.newRun
    run.compileFromString(source)
    run.runContext
  }

  def checkTypes(source: String, typeStrings: String*)(assertion: (List[Type], Context) => Unit): Unit =
    checkTypes(source, List(typeStrings.toList)) { (tpess, ctx) => (tpess: @unchecked) match {
      case List(tpes) => assertion(tpes, ctx)
    }}

  def checkTypes(source: String, typeStringss: List[List[String]])(assertion: (List[List[Type]], Context) => Unit): Unit = {
    val dummyName = "x_x_x"
    val vals = typeStringss.flatten.zipWithIndex.map{case (s, x)=> s"val ${dummyName}$x: $s = ???"}.mkString("\n")
    val gatheredSource = s" ${source}\n object A$dummyName {$vals}"
    checkCompile("frontend", gatheredSource) {
      (tree, context) =>
        implicit val ctx = context
        val findValDef: (List[tpd.ValDef], tpd.Tree) => List[tpd.ValDef] =
          (acc , tree) =>  { tree match {
          case t: tpd.ValDef if t.name.startsWith(dummyName) => t :: acc
          case _ => acc
        }
      }
      val d = new tpd.DeepFolder[List[tpd.ValDef]](findValDef).foldOver(Nil, tree)
      val tpes = d.map(_.tpe.widen).reverse
      val tpess = typeStringss.foldLeft[(List[Type], List[List[Type]])]((tpes, Nil)) {
        case ((rest, result), typeStrings) =>
          val (prefix, suffix) = rest.splitAt(typeStrings.length)
          (suffix, prefix :: result)
      }._2.reverse
      assertion(tpess, context)
    }
  }

  def methType(names: String*)(paramTypes: Type*)(resultType: Type = defn.UnitType) =
    MethodType(names.toList map (_.toTermName), paramTypes.toList, resultType)
}
