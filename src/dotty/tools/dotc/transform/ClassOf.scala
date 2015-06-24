package dotty.tools.dotc
package transform

import ast.tpd
import core.Constants.Constant
import core.Contexts.Context
import core.StdNames.nme
import core.Symbols.TermSymbol
import core.TypeErasure
import TreeTransforms.{MiniPhaseTransform, TransformerInfo, TreeTransform}

/** Rewrite `classOf` calls as follow:
 *
 *  For every primitive class C whose boxed class is called B:
 *    classOf[C]    -> B.TYPE
 *  For every non-primitive class D:
 *    classOf[D]    -> Literal(Constant(erasure(D)))
 */
class ClassOf extends MiniPhaseTransform {
  import tpd._

  override def phaseName: String = "classOf"

  private var classOfMethod: TermSymbol = _

  override def prepareForUnit(tree: tpd.Tree)(implicit ctx: Context): TreeTransform = {
    val predefModule = ctx.definitions.ScalaPredefModule
    classOfMethod = ctx.requiredMethod(predefModule.moduleClass.asClass, nme.classOf)
    this
  }

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (tree.symbol eq classOfMethod) {
      val tp = tree.args.head.tpe
      val defn = ctx.definitions
      val claz = tp.classSymbol

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
        case _                 => Literal(Constant(TypeErasure.erasure(tp)))
      }
    } else tree
  }
}
