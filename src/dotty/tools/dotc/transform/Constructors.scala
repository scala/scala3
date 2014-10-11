package dotty.tools.dotc.transform

import TreeTransforms._
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames._

/** This transform moves initializers from body to constructor.
 *  Right now it's a dummy.
 *  Awaiting for real implemetation
 */
class Constructors extends MiniPhaseTransform {

  override def phaseName: String = "constructors"
  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if(tree.symbol.isClassConstructor) {
      val claz = tree.symbol.enclosingClass.asClass
      val zuper = claz.info.parents.head.typeSymbol
      cpy.DefDef(tree)(rhs = {
        val parentCall =
          Super(This(claz), tpnme.EMPTY, true).select(zuper.primaryConstructor).appliedToNone
        if(tree.rhs.isEmpty) parentCall
        else Block(List(parentCall), tree.rhs)
      })
    } else tree
  }
}