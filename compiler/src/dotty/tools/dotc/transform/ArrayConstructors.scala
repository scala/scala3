package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts._
import Symbols._
import Types._
import StdNames._
import ast.Trees._
import dotty.tools.dotc.ast.tpd
import util.Lst
import util.Lst.{NIL, +:, toLst}

/** This phase rewrites calls to array constructors to newArray method in Dotty.runtime.Arrays module.
 *
 * It assummes that generic arrays have already been handled by typer(see Applications.convertNewGenericArray).
 * Additionally it optimizes calls to scala.Array.ofDim functions by replacing them with calls to newArray with specific dimensions
 */
class ArrayConstructors extends MiniPhase {
  import ast.tpd._

  override def phaseName: String = "arrayConstructors"

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree = {
    def expand(elemType: Type, dims: Lst[Tree]) =
      tpd.newArray(elemType, tree.tpe, tree.span, JavaSeqLiteral(dims, TypeTree(defn.IntClass.typeRef)))

    if (tree.fun.symbol eq defn.ArrayConstructor) {
      val TypeApply(tycon, Lst(targ)) = tree.fun
      expand(targ.tpe, tree.args)
    }
    else if ((tree.fun.symbol.maybeOwner eq defn.ArrayModule.moduleClass) && (tree.fun.symbol.name eq nme.ofDim) && !tree.tpe.isInstanceOf[MethodicType]) {
      val Apply(Apply(TypeApply(_, Lst(tp)), _), _) = tree
      val cs = tp.tpe.classSymbol
      tree.fun match {
        case Apply(TypeApply(t: Ident, targ), dims)
          if !TypeErasure.isGeneric(targ.head.tpe) && !ValueClasses.isDerivedValueClass(cs) =>
          expand(targ.head.tpe, dims)
        case Apply(TypeApply(t: Select, targ), dims)
          if !TypeErasure.isGeneric(targ.head.tpe) && !ValueClasses.isDerivedValueClass(cs) =>
          Block(Lst(t.qualifier), expand(targ.head.tpe, dims))
        case _ => tree
      }
    }

    else tree
  }
}
