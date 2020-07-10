package dotty.tools.dotc
package transform

import ast.tpd
import core._
import Contexts._, Symbols._
import DenotTransformers._, MegaPhase._
import TreeExtractors._, ValueClasses._

/** This phase elides unnecessary value class allocations
 *
 *  For a value class V defined as:
 *    class V(val underlying: U) extends AnyVal
 *  we avoid unnecessary allocations:
 *     new V(u1) == new V(u2) => u1 == u2   provided V does not redefine `equals`
 *    (new V(u)).underlying() => u
 */
class VCElideAllocations extends MiniPhase with IdentityDenotTransformer {
  import tpd._

  override def phaseName: String = "vcElideAllocations"

  override def runsAfter: Set[String] = Set(ElimErasedValueType.name)

  override def transformApply(tree: Apply)(using Context): Tree =
    tree match {
      // new V(u1) == new V(u2) => u1 == u2
      // (We don't handle != because it has been eliminated by InterceptedMethods)
      case BinaryOp(NewWithArgs(tp1, List(u1)), op, NewWithArgs(tp2, List(u2)))
      if (tp1 eq tp2) && (op eq defn.Any_==) &&
         isDerivedValueClass(tp1.typeSymbol) &&
         !defn.Any_equals.overridingSymbol(tp1.typeSymbol.asClass).exists =>
        // == is overloaded in primitive classes
        u1.equal(u2)

      // (new V(u)).underlying() => u
      case ValueClassUnbox(NewWithArgs(_, List(u))) =>
        u

      case _ =>
        tree
    }
}
