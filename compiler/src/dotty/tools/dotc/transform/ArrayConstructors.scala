package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts._
import Symbols._
import Types._
import StdNames._
import dotty.tools.dotc.ast.tpd


import scala.collection.immutable.::


/** This phase rewrites calls to array constructors to newArray method in Dotty.runtime.Arrays module.
 *
 * It assummes that generic arrays have already been handled by typer(see Applications.convertNewGenericArray).
 * Additionally it optimizes calls to scala.Array.ofDim functions by replacing them with calls to newArray with specific dimensions
 */
class ArrayConstructors extends MiniPhase {
  import ast.tpd._

  override def phaseName: String = ArrayConstructors.name

  override def description: String = ArrayConstructors.description

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree = {
    def expand(elemType: Type, dims: List[Tree]) =
      tpd.newArray(elemType, tree.tpe, tree.span, JavaSeqLiteral(dims, TypeTree(defn.IntClass.typeRef)))

    if (tree.fun.symbol eq defn.ArrayConstructor) {
      val TypeApply(tycon, targ :: Nil) = tree.fun: @unchecked
      expand(targ.tpe, tree.args)
    }
    else if ((tree.fun.symbol.maybeOwner eq defn.ArrayModuleClass) && (tree.fun.symbol.name eq nme.ofDim) && !tree.tpe.isInstanceOf[MethodicType]) {
      val Apply(Apply(TypeApply(_, List(tp)), _), _) = tree: @unchecked
      val cs = tp.tpe.classSymbol
      tree.fun match {
        case Apply(TypeApply(t: Ident, targ), dims)
          if !TypeErasure.isGeneric(targ.head.tpe) && !ValueClasses.isDerivedValueClass(cs) =>
          expand(targ.head.tpe, dims)
        case Apply(TypeApply(t: Select, targ), dims)
          if !TypeErasure.isGeneric(targ.head.tpe) && !ValueClasses.isDerivedValueClass(cs) =>
          Block(t.qualifier :: Nil, expand(targ.head.tpe, dims))
        case _ => tree
      }
    }

    else tree
  }
}

object ArrayConstructors:
  val name: String = "arrayConstructors"
  val description: String = "intercept creation of (non-generic) arrays and intrinsify"
