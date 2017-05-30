package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Contexts._
import ast.Trees._
import ast.tpd
import core.Constants.Constant
import core.StdNames._
import core.Symbols._


class TransformWildcards extends MiniPhaseTransform with IdentityDenotTransformer { thisTransform =>
  import tpd._

  override def phaseName = "transformWildcards"

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    tree match {
      case ValDef(_, _, Ident(name)) => assert(name ne nme.WILDCARD)
      case _ =>
    }
  }

  override def transformValDef(tree: ValDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    tree.rhs match {
      case Ident(nme.WILDCARD) =>
        val literal = defaultValueLiteral(tree)
        cpy.ValDef(tree)(rhs = literal)
      case _ => tree
    }
  }

  def defaultValueLiteral(tree: ValDef)(implicit ctx: Context) = {
    val t = tree.rhs.tpe.widenDealias.classSymbol

    if (t eq defn.DoubleClass) Literal(Constant(0.0d))
    else if (t eq defn.IntClass) Literal(Constant(0))
    else if (t eq defn.CharClass) Literal(Constant('\u0000'))
    else if (t eq defn.ByteClass) Literal(Constant(0: Byte))
    else if (t eq defn.LongClass) Literal(Constant(0L))
    else if (t eq defn.FloatClass) Literal(Constant(0f))
    else if (t eq defn.UnitClass) Literal(Constant(()))
    else if (t eq defn.BooleanClass) Literal(Constant(false))
    else Literal(Constant(null))
  }
}