package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts.Context
import Flags._
import SymUtils._
import Symbols._
import SymDenotations._
import Types._
import Decorators._
import DenotTransformers._
import StdNames._
import NameOps._
import ast.Trees._
import dotty.tools.dotc.ast.tpd
import util.Positions._
import Names._

import collection.mutable
import ResolveSuper._

import scala.collection.immutable.::


/** This phase rewrites calls to array constructors to newArray method in Dotty.runtime.Arrays module.
 *
 * It assummes that generic arrays have already been handled by typer(see Applications.convertNewGenericArray).
 * Additionally it optimizes calls to scala.Array.ofDim functions by replacing them with calls to newArray with specific dimensions
 */
class ArrayConstructors extends MiniPhase {
  import ast.tpd._

  override def phaseName: String = "arrayConstructors"

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context): tpd.Tree = {
    def rewrite(elemType: Type, dims: List[Tree]) =
      tpd.newArray(elemType, tree.tpe, tree.pos, JavaSeqLiteral(dims, TypeTree(defn.IntClass.typeRef)))

    if (tree.fun.symbol eq defn.ArrayConstructor) {
      val TypeApply(tycon, targ :: Nil) = tree.fun
      rewrite(targ.tpe, tree.args)
    } else if ((tree.fun.symbol.maybeOwner eq defn.ArrayModule) && (tree.fun.symbol.name eq nme.ofDim) && !tree.tpe.isInstanceOf[MethodicType]) {
      val Apply(Apply(TypeApply(_, List(tp)), _), _) = tree
      val cs = tp.tpe.widen.classSymbol
      tree.fun match {
        case Apply(TypeApply(t: Ident, targ), dims)
          if !TypeErasure.isUnboundedGeneric(targ.head.tpe) && !ValueClasses.isDerivedValueClass(cs) =>
          rewrite(targ.head.tpe, dims)
        case Apply(TypeApply(t: Select, targ), dims)
          if !TypeErasure.isUnboundedGeneric(targ.head.tpe) && !ValueClasses.isDerivedValueClass(cs) =>
          Block(t.qualifier :: Nil, rewrite(targ.head.tpe, dims))
        case _ => tree
      }

    } else tree
  }
}
