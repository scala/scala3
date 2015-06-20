package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core._, core.Decorators._
import Contexts._, Flags._, Trees._, StdNames._, Symbols._
import DenotTransformers._, TreeTransforms._, Phases.Phase
import ExtensionMethods._, TreeExtractors._, ValueClasses._

/** This phase elides unnecessary value class allocations
 *
 *  For a value class V defined as:
 *    class V(val underlying: U) extends AnyVal
 *  we avoid unnecessary allocations:
 *    (new V(u)).underlying() => u
 *
 *  If V was compiled by Scala 2, we also optimize equality comparisons:
 *     new V(u1) == new V(u2) => u1 == u2
 *  This is not needed for classes compiled by Dotty because of [[VCEqMethods]].
 */
class VCElideAllocations extends MiniPhaseTransform with IdentityDenotTransformer {
  import tpd._

  override def phaseName: String = "vcElideAllocations"

  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[ElimErasedValueType])

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree =
    tree match {
      // If V was compiled by Scala 2:
      // new V(u1) == new V(u2) => u1 == u2
      // (We don't handle != because it has been eliminated by InterceptedMethods)
      case BinaryOp(NewWithArgs(tp1, List(u1)), op, NewWithArgs(tp2, List(u2)))
      if (tp1 eq tp2) && (op eq defn.Any_==) && isDerivedValueClass(tp1.typeSymbol)
          && tp1.typeSymbol.is(Scala2x) =>
        // == is overloaded in primitive classes
        applyOverloaded(u1, nme.EQ, List(u2), Nil, defn.BooleanType)

      // (new V(u)).underlying() => u
      case ValueClassUnbox(NewWithArgs(_, List(u))) =>
        u

      case _ =>
        tree
    }
}
