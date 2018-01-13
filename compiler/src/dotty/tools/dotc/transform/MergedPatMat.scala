package dotty.tools.dotc
package transform

import core.Symbols._
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

class MergedPatMat extends MiniPhase with InfoTransformer {
  import dotty.tools.dotc.ast.tpd._

  def phaseName: String = "mergedPatMat"

  override def runsAfter = Set(classOf[ElimRepeated])

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    tcp.checkPostCondition(tree)
  }

  override def changesMembers = true

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

  override def prepareForUnit(tree: Tree)(implicit ctx: Context) = {
    si.prepareForUnit(tree)
  }

  override def transformTry(tree: Try)(implicit ctx: Context): Try = {
    tcp.transformTry(tree)
  }

  override def transformTemplate(tree: Template)(implicit ctx: Context): Template = {
    eo.transformTemplate(tree)
  }

  override def transformSelect(tree: Select)(implicit ctx: Context): Tree = {
    val tree1 = si.transformSelect(es.transformSelect(tree))
    tree1 match {
      case tree1: Select =>
        cca.transformSelect(tree1)
      case tree1: Apply =>
        s.transformApply(tree1)
      case tree1: TypeApply =>
        s.transformTypeApply(tree1)
      case _ => // Block | Ident
        tree1
    }
  }

  override def transformIdent(tree: Ident)(implicit ctx: Context): Tree = {
    es.transformIdent(tree) // This | Ident
  }


  override def transformClosure(tree: Closure)(implicit ctx: Context): Closure = {
    eo.transformClosure(tree)
  }

  override def transformMatch(tree: Match)(implicit ctx: Context): Tree = {
    val tree1 = pm.transformMatch(tree)
    // assert(!(
    //   tree1.isInstanceOf[Template] ||
    //   tree1.isInstanceOf[Select] ||
    //   tree1.isInstanceOf[Ident] ||
    //   tree1.isInstanceOf[Closure] ||
    //   tree1.isInstanceOf[DefDef] ||
    //   tree1.isInstanceOf[TypeApply] ||
    //   tree1.isInstanceOf[Apply]
    // ))
    tree1
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context) = {
    si.transformDefDef(tree) // DefDef | Thicket[DefDef]
  }

  // Last mini-phase only

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context) = {
    s.transformTypeApply(tree)
  }
  override def transformApply(tree: Apply)(implicit ctx: Context) = {
    s.transformApply(tree)
  }
}
