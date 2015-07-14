package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core._, core.Decorators._
import Contexts._, Trees._, Types._, StdNames._, Symbols._
import Constants.Constant
import DenotTransformers._, TreeTransforms._, Phases.Phase
import TypeErasure.ErasedValueType, ValueClasses._

/** This phase erases arrays of value classes to their runtime representation.
 *
 *  For a value class V whose erased underlying type is U, an array of V has type
 *  Array[V] before Erasure and Array[ErasedValueType(V, U)] afterwards. This phase
 *  replaces this type by VCXArray where X is "U" if U is a primitive type and is "Object"
 *  otherwise.
 */
class VCArrays extends MiniPhaseTransform with InfoTransformer {
  import tpd._

  override def phaseName: String = "vcArrays"

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type =
    eraseVCArrays(tp)

  private def eraseVCArrays(tp: Type)(implicit ctx: Context): Type = tp match {
    case JavaArrayType(ErasedValueType(cls, _)) =>
      defn.vcArrayOf(cls).typeRef
    case tp: MethodType =>
      val paramTypes = tp.paramTypes.mapConserve(eraseVCArrays)
      val retType = eraseVCArrays(tp.resultType)
      tp.derivedMethodType(tp.paramNames, paramTypes, retType)
    case _ =>
      tp
  }

  private def transformTypeOfTree(tree: Tree)(implicit ctx: Context): Tree =
    tree.withType(eraseVCArrays(tree.tpe))

  override def transformValDef(tree: ValDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val tpt1 = transformTypeOfTree(tree.tpt)
    cpy.ValDef(tree)(tpt = tpt1)
  }
  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val tpt1 = transformTypeOfTree(tree.tpt)
    cpy.DefDef(tree)(tpt = tpt1)
  }

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree =
    tree match {
      case TypeApply(sel @ Select(_, _), _) if (sel.symbol == defn.newRefArrayMethod) =>
        // Preserve the semi-erased type of the array so that we can properly transform
        // it in transformApply
        tree
      case TypeApply(fun, args) =>
        val tree1 = cpy.TypeApply(tree)(fun, args.map(transformTypeOfTree(_)))
        transformTypeOfTree(tree1)
    }

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    tree match {
      // vcArray(xs, clsTp) => new VCXArray(xs, mod)
      case Apply(qual, List(xs, Literal(Constant(clsTp: Type))))
        if qual.symbol == defn.vcArrayMethod =>
        val cls = clsTp.classSymbol.asClass
        val mod = cls.companionModule
        New(defn.vcArrayOf(cls).typeRef, List(xs, ref(mod)))

      // newRefArray[ErasedValueType(V, U)[]](args) => New VCXArray(newXArray(args), V)
      case Apply(ta @ TypeApply(sel @ Select(_,_), List(targ)), args)
          if (sel.symbol == defn.newRefArrayMethod) =>
        targ.tpe match {
          case JavaArrayType(ErasedValueType(cls, underlying)) =>
            val mod = cls.companionModule
            New(defn.vcArrayOf(cls).typeRef,
              List(newArray(TypeTree(underlying), tree.pos).appliedToArgs(args),
                ref(mod)))
          case _ =>
            tree
        }
      // array.[]update(idx, elem) => array.arr().[]update(idx, elem)
      case Apply(Select(array, nme.primitive.arrayUpdate), List(idx, elem)) =>
        if (defn.vcArrayValues.contains(array.tpe.widen.classSymbol)) {
          array.select(nme.ARR).appliedToNone
            .select(nme.primitive.arrayUpdate).appliedTo(idx, elem)
        } else tree
      // array.[]apply(idx) => array.arr().[]apply(idx)
      case Apply(sel @ Select(array, nme.primitive.arrayApply), List(idx)) =>
        if (defn.vcArrayValues.contains(array.tpe.widen.classSymbol)) {
            array.select(nme.ARR).appliedToNone
              .select(nme.primitive.arrayApply).appliedTo(idx)
        } else tree
      // array.[]length() => array.arr().[]length()
      case Apply(Select(array, nme.primitive.arrayLength), Nil) =>
        if (defn.vcArrayValues.contains(array.tpe.widen.classSymbol)) {
          array.select(nme.ARR).appliedToNone
            .select(nme.primitive.arrayLength).appliedToNone
        } else tree
      case _ =>
        tree
    }
  }
}
