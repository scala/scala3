package dotty.tools.dotc
package transform

import ast.tpd
import core.Contexts.Context
import core.StdNames.nme
import core.Symbols.TermSymbol
import core.Phases.Phase
import TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/** Rewrite `getClass` calls as follow:
  *
  *  For every instance of primitive class C whose boxed class is called B:
  *    instanceC.getClass    -> B.TYPE
  *  For every instance of non-primitive class D:
  *    instanceD.getClass    -> instanceD.getClass
  */
class GetClass extends MiniPhaseTransform {
  import tpd._

  override def phaseName: String = "getClass"

  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[Erasure])

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    import ast.Trees._

    tree match {
      case Apply(Select(qual, nme.getClass_), Nil) =>
        val defn = ctx.definitions
        val claz = qual.tpe.classSymbol

        def TYPE(module: TermSymbol) = ref(module).select(nme.TYPE_).ensureConforms(tree.tpe)
        claz match {
          case defn.BooleanClass => TYPE(defn.BoxedBooleanModule)
          case defn.ByteClass    => TYPE(defn.BoxedByteModule)
          case defn.ShortClass   => TYPE(defn.BoxedShortModule)
          case defn.CharClass    => TYPE(defn.BoxedCharModule)
          case defn.IntClass     => TYPE(defn.BoxedIntModule)
          case defn.LongClass    => TYPE(defn.BoxedLongModule)
          case defn.FloatClass   => TYPE(defn.BoxedFloatModule)
          case defn.DoubleClass  => TYPE(defn.BoxedDoubleModule)
          case defn.UnitClass    => TYPE(defn.BoxedVoidModule)
          case _                 => tree
        }
      case _ => tree
    }
  }
}
