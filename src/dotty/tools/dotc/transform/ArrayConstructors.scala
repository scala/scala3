package dotty.tools.dotc
package transform

import core._
import TreeTransforms._
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


/** This phase rewrites calls to array constructors to newArray and newGenericArray methods
  * in Dotty.runtime.Arrays module.
  *
  * Additionally it optimizes calls to scala.Array.ofDim functions
  *
  */
class ArrayConstructors extends MiniPhaseTransform { thisTransform =>
  import ast.tpd._

  override def phaseName: String = "arrayConstructors"

  override def transformApply(tree: tpd.Apply)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    def rewrite(elemType: Type, dims: List[Tree]) =
      tpd.newArray(elemType, tree.tpe, tree.pos, JavaSeqLiteral(dims, TypeTree(defn.IntClass.typeRef)).asInstanceOf[JavaSeqLiteral])

    if (tree.fun.symbol eq defn.ArrayConstructor) {
      tree.fun match {
        case TypeApply(tycon, targ :: Nil)  =>
          rewrite(targ.tpe, tree.args)
        case _ =>
          ???
      }
    } else if ((tree.fun.symbol.maybeOwner eq defn.ArrayModule) && (tree.fun.symbol.name eq nme.ofDim) && !tree.tpe.isInstanceOf[MethodicType]) {

      tree.fun match {
        case Apply(TypeApply(t: Ident, targ), dims) =>
          rewrite(targ.head.tpe, dims)
        case Apply(TypeApply(t: Select, targ), dims) =>
          Block(t.qualifier :: Nil, rewrite(targ.head.tpe, dims))
        case _ => tree
      }

    } else tree
  }
}

