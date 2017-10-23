package dotty.tools.backend.jvm

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.transform._
import dotty.tools.dotc.transform.TreeTransforms._

import scala.collection.mutable
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._

/**
 * Verifies that each Label DefDef has only a single address to jump back and
 * reorders them such that they are not nested and this address is a
 * fall-through address for the JVM.
 *
 * ```scala
 * <label> def foo(i: Int) = {
 *   <label> def bar = 0
 *   <label> def dough(i: Int) = if (i == 0) bar else foo(i-1)
 *   dough(i)
 * }
 *
 * foo(100)
 * ```
 *
 * will get rewritten to:
 *
 * ```scala
 * <label> def foo(i: Int) = dough(i)
 * <label> def dough(i: Int) = if (i == 0) bar else foo(i-1)
 * <label> def bar = 2
 *   foo(100)
 * ```
 *
 * Proposed way to generate this pattern in backend is:
 *
 * ```scala
 * foo(100)
 * <jump foo>
 * <label> def foo(i: Int) = dough(i)
 * // <jump a>                           // unreachable
 * <label> def dough(i: Int) = if (i == 0) bar else foo(i-1)
 * // <jump a>                           // unreachable
 * <label> def bar = 2
 * // <jump a>                           // unreachable
 * <asm point a>
 * ```
 *
 * Unreachable jumps will be eliminated by local dead code analysis.
 * After JVM is smart enough to remove next-line jumps
 *
 * Note that his phase Ychecking this phase required softening scoping rules
 * as it intentionally allowed to break scoping rules inside methods for labels.
 * This is modified by setting `labelsReordered` flag in Phases.
 *
 * @author Dmitry Petrashko
 * @author Nicolas Stucki
 */
class LabelDefs extends MiniPhaseTransform {
  import tpd._

  def phaseName: String = "labelDef"

  override def runsAfterGroupsOf = Set(classOf[Flatten])

  private val labelDefs: mutable.HashMap[Symbol, DefDef] = new mutable.HashMap[Symbol, DefDef]()

  override def prepareForDefDef(tree: DefDef)(implicit ctx: Context): TreeTransform = {
    if (isWhileDef(tree)) this
    else if (tree.symbol.is(Label)) NoTransform // transformation is done in transformApply
    else {
      collectLabelDefs(tree.rhs)
      this
    }
  }

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (!tree.symbol.is(Label)) tree
    else labelDefs.get(tree.symbol) match {
      case Some(labelDef) =>
        labelDefs -= tree.symbol
        val newRhs = transform(labelDef.rhs) // transform the body of the label def
        val transformedLabelDef = cpy.DefDef(labelDef)(rhs = newRhs)
        Block(transformedLabelDef :: Nil, tree)
      case None => tree
    }
  }

  override def transformBlock(tree: Block)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (tree.stats.isEmpty || !tree.stats.exists(_.symbol.is(Label))) tree
    else {
      // Only keep label defs if they are followed by their applies
      val newStats = tree.stats.zipWithConserve(tree.stats.tail :+ tree.expr) { (stat, next) =>
        if (!stat.symbol.is(Label) || stat.symbol == next.symbol) stat
        else EmptyTree
      }.filterNot(_.isEmpty)
      seq(newStats, tree.expr)
    }
  }

  private def collectLabelDefs(tree: Tree)(implicit ctx: Context): Unit = new TreeTraverser() {
    assert(labelDefs.isEmpty)
    override def traverse(tree: Tree)(implicit ctx: Context): Unit = tree match {
      case _: Template =>
      case tree: DefDef if !isWhileDef(tree) =>
        assert(tree.symbol.is(Label))
        labelDefs(tree.symbol) = tree
        traverseChildren(tree)
      case _ => traverseChildren(tree)
    }
  }.traverse(tree)

  private def isWhileDef(ddef: DefDef)(implicit ctx: Context): Boolean = {
    ddef.symbol.is(Label) &&
    (ddef.name == nme.WHILE_PREFIX || ddef.name == nme.DO_WHILE_PREFIX)
  }
}
