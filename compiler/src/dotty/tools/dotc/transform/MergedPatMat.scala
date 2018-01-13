package dotty.tools.dotc
package transform

import core.Symbols._
import core.Phases._
import core.DenotTransformers._
import core.StdNames._
import ast.Trees._
import core.Types._
import core.NameKinds.ExceptionBinderName
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.util.Positions.Position

class MergedPatMat extends MacroTransform with InfoTransformer {
  import dotty.tools.dotc.ast.tpd._

  def phaseName: String = "mergedPatMat"

  override def runsAfter = Set(classOf[ElimRepeated])

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    tcp.checkPostCondition(tree)
  }

  override def changesMembers = true

  override def transformPhase(implicit ctx: Context) = this.next

  protected def newTransformer(implicit ctx: Context): Transformer =
    new MergedPatMatTransformer

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context) = {
    eo.transformInfo(tp, sym)
  }
  override def mayChange(sym: Symbol)(implicit ctx: Context): Boolean = {
    eo.mayChange(sym)
  }

  val tcp = new TryCatchPatterns(this)
  val pm = new PatternMatcher(this)
  val eo = new ExplicitOuter(this)
  val es = new ExplicitSelf
  val si = new ShortcutImplicits(this)
  val cca = new CrossCastAnd
  val s = new Splitter

  val transformer = new MergedPatMatTransformer

  // Used in PatternMatcher
  def transformOuter(tree: Tree)(implicit ctx: Context): Tree = {
    transformer.transform(tree)
  }

  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    si.prepareForUnit(unit.tpdTree)(ctx.withPhase(transformPhase))
    super.run(ctx)
  }

  class MergedPatMatTransformer extends Transformer {
    override def transform(tree: Tree)(implicit ctx: Context): Tree = {
      val tree1 = super.transform(tree)
      tree1 match {
        case tree1: Try =>
          tcp.transformTry(tree1)
        case tree1: Template =>
          eo.transformTemplate(tree1)
        case tree1: Select =>
          val tree2 = si.transformSelect(es.transformSelect(tree1))
          tree2 match {
            case tree2: Select =>
              cca.transformSelect(tree2)
            case tree2: Apply =>
              s.transformApply(tree2)
            case tree2: TypeApply =>
              s.transformTypeApply(tree2)
            case _ => // Block | Ident
              tree2
          }
        case tree1: Ident =>
          es.transformIdent(tree1) // This | Ident
        case tree1: Closure =>
          es.transformClosure(tree1)
        case tree1: Match =>
          val tree2 = pm.transformMatch(tree1)
          // assert(!(
          //   tree2.isInstanceOf[Template] ||
          //   tree2.isInstanceOf[Select] ||
          //   tree2.isInstanceOf[Ident] ||
          //   tree2.isInstanceOf[Closure] ||
          //   tree2.isInstanceOf[DefDef] ||
          //   tree2.isInstanceOf[TypeApply] ||
          //   tree2.isInstanceOf[Apply]
          // ))
          tree2
        case tree1: DefDef =>
          si.transformDefDef(tree1) // DefDef | Thicket[DefDef]

        // Last mini-phase only

        case tree1: TypeApply =>
          s.transformTypeApply(tree1)
        case tree1: Apply =>
          s.transformApply(tree1)
        case _ =>
          tree1
      }
    }
  }
}
