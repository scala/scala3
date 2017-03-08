package dotty.tools.dotc
package transform

import core._
import DenotTransformers._
import Phases.Phase
import Contexts.Context
import Symbols._
import Constants._
import TreeTransforms._
import Flags._

/** Erases all ValDefs of phantom type,
 *  except for `lazy val` as the field are not yet generated.
 *
 *    <accessor> <mods> def x(): ErasedPhantom = e
 *      --> <accessor> <mods> def x(): ErasedPhantom = null
 *
 *
 *    x = e  --> e  where type of x is ErasedPhantom
 *
 *    Filed in class
 *    <private> <mods> val x: ErasedPhantom --> EmptyTree
 *
 *    Field in method
 *    <mods> val x: ErasedPhantom = e --> e
 */
 class PhantomVals extends MiniPhaseTransform with IdentityDenotTransformer { thisTransform =>
  import ast.tpd._

  override def phaseName = "phantomVals"

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    // TODO
  }

  override def runsAfterGroupsOf: Set[Class[_ <: Phase]] = Set(classOf[Constructors])

  override def transformValDef(tree: ValDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val sym = tree.symbol
    if (sym.is(Lazy) || !(tree.tpt.tpe =:= defn.ErasedPhantomType)) tree
    else if (sym.is(Private)) EmptyTree
    else tree.rhs
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val sym = tree.symbol
    if (sym.isGetter && !sym.is(Lazy) && (tree.tpt.tpe =:= defn.ErasedPhantomType)) {
      cpy.DefDef(tree)(rhs = Literal(Constant(null)).withType(defn.ErasedPhantomType))
    } else tree
  }

  override def transformAssign(tree: Assign)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val lhsSym = tree.lhs.symbol
    if (!lhsSym.is(Lazy) && tree.rhs.tpe =:= defn.ErasedPhantomType) tree.rhs
    else tree
  }

}
