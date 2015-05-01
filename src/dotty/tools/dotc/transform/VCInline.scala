package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core._, core.Decorators._
import Contexts._, Trees._, StdNames._, Symbols._
import DenotTransformers._, TreeTransforms._, Phases.Phase
import ExtensionMethods._, TreeExtractors._, ValueClasses._

/** This phase inlines calls to methods and fields of value classes.
 *
 *  For a value class V defined as:
 *    case class V(val underlying: U) extends AnyVal
 *  We replace method calls by calls to the corresponding extension method:
 *    v.foo(args) => V.foo$extension(v.underlying(), args)
 *  And we avoid unnecessary allocations:
 *     new V(u1) == new V(u2) => u1 == u2
 *    (new V(u)).underlying() => u
 */
class VCInline extends MiniPhaseTransform with IdentityDenotTransformer {
  import tpd._

  override def phaseName: String = "vcInline"

  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[ElimErasedValueType])

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree =
    tree match {
      // new V(u1) == new V(u2) => u1 == u2
      // (We don't handle != because it has been eliminated by InterceptedMethods)
      case BinaryOp(NewWithArgs(tp1, List(u1)), op, NewWithArgs(tp2, List(u2)))
      if (tp1 eq tp2) && (op eq defn.Any_==) && isDerivedValueClass(tp1.typeSymbol) =>
        // == is overloaded in primitive classes
        applyOverloaded(u1, nme.EQ, List(u2), Nil, defn.BooleanType)

      // (new V(u)).underlying() => u
      case ValueClassUnbox(NewWithArgs(_, List(u))) =>
        u

      // (new V(u)).foo(args) => V.foo$extension(u, args)
      //          v.foo(args) => V.foo$extension(v.underlying(), args)
      case Apply(sel @ Select(receiver, _), args) =>
        val classMeth = sel.symbol
        if (isMethodWithExtension(classMeth)) {
          val classSym = receiver.tpe.widenDealias.typeSymbol.asClass
          val unboxedReceiver = receiver match {
            case NewWithArgs(_, List(u)) =>
              u
            case _ =>
              receiver.select(valueClassUnbox(classSym)).appliedToNone
          }
          val extensionMeth = extensionMethod(classMeth)
          ref(extensionMeth).appliedToArgs(unboxedReceiver :: args)
        } else tree

      case _ =>
        tree
    }
}
