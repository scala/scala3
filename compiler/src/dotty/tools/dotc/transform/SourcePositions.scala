package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.MegaPhase._

/** Replace references of dotty.source.Position.XYZ by their call site positions **/
class SourcePositions extends MiniPhase {
  import tpd._

  def phaseName: String = "sourcePositions"

  override def transformSelect(tree: Select)(implicit ctx: Context): Tree =
    transformPosition(tree)

  override def transformIdent(tree: Ident)(implicit ctx: Context): Tree =
    transformPosition(tree)

  private def transformPosition(tree: RefTree)(implicit ctx: Context): Tree = {
    if (!tree.symbol.exists) tree
    else if (tree.symbol.name == nme.thisLine && tree.symbol.owner == defn.DottySourceLineNumberModuleClass)
      newPositionValue(tree, tree.pos.line + 1)
    else if (tree.symbol.name == nme.thisSource && tree.symbol.owner == defn.DottySourceSourcePathModuleClass)
      newPositionValue(tree, ctx.compilationUnit.source.path)
    else tree
  }

  private def newPositionValue(tree: Tree, value: Any)(implicit ctx: Context): Tree = {
    val positionValueType = tree.tpe.widenDealias
    val valueArg = List(Literal(Constant(value)))
    New(positionValueType).select(nme.CONSTRUCTOR).appliedToArgs(valueArg)
  }
}
