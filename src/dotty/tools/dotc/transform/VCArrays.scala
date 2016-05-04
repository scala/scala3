package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core._, core.Decorators._
import Contexts._, Trees._, Types._, StdNames._, Symbols._
import Constants.Constant
import DenotTransformers._, TreeTransforms._, Phases.Phase
import TypeErasure.ErasedValueType, ValueClasses._
import dotty.tools.dotc.core.SymDenotations.ClassDenotation

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

  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = eraseVCArrays(tp)

  private def eraseVCArrays(tp: Type)(implicit ctx: Context): Type = {
    val transform = new TypeMap {
      //TODO: rewrite, don't create TypeMap for each invocation of eraseVCArrays
      def apply(tp: Type) = mapOver {
        tp match {
          case JavaArrayType(ErasedValueType(tr, _)) =>
            val cls = tr.symbol.asClass
            defn.vcArrayOf(cls).typeRef
          case _ =>
            tp
        }
      }
    }
    transform(tp)
  }

  private def transformTypeOfTree(tree: Tree)(implicit ctx: Context): Tree =
    tree.withType(eraseVCArrays(tree.tpe))

  override def transformValDef(tree: ValDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val tpt1 = transformTypeOfTree(tree.tpt)
    cpy.ValDef(tree)(tpt = tpt1)
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val vc = tree.symbol.owner.companionClass
    val newRhs = tree.name match {
      //box body implementation
      case _ if tree.name == nme.box && ValueClasses.isDerivedValueClass(tree.symbol.owner.companionClass) && !tree.symbol.is(Flags.Bridge) =>
        val List(List(param)) = tree.vparamss
        val newParam = ref(param.symbol).ensureConforms(ValueClasses.underlyingOfValueClass(vc.asClass))
        val newRhs = New(vc.typeRef, newParam :: Nil).ensureConforms(tree.tpt.tpe)
        newRhs
      case _ => tree.rhs
    }
    val tpt1 = transformTypeOfTree(tree.tpt)
    val newRhs2 = transformTypeOfTree(newRhs)
    cpy.DefDef(tree)(tpt = tpt1, rhs = newRhs2)
  }

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree =
    tree match {
//      case TypeApply(sel @ Select(_, _), _) if (sel.symbol == defn.newRefArrayMethod) =>
//        // Preserve the semi-erased type of the array so that we can properly transform
//        // it in transformApply
//        tree
      case TypeApply(fun, args) =>
        val tree1 = cpy.TypeApply(tree)(fun, args.map(transformTypeOfTree(_)))
        transformTypeOfTree(tree1)
    }

  override def transformSeqLiteral(tree: SeqLiteral)(implicit ctx: Context, info: TransformerInfo): Tree =
    tree.tpe match {
      // [arg1, arg2,  ...] => new VCXArray([V.evt2u$(arg1), V.evt2u$(arg2), ...])
      case JavaArrayType(ErasedValueType(tr, tund)) =>
        val cls = tr.symbol.asClass
        val evt2uMethod = ref(evt2u(cls))
        //[V.evt2u$(arg1), V.evt2u$(arg2), ...]
        val underlyingArray = JavaSeqLiteral(tree.elems.map(evt2uMethod.appliedTo(_)), TypeTree(tund))
        val mod = cls.companionModule
        //new VCXArray([V.evt2u$(arg1), V.evt2u$(arg2), ...], VCXCompanion)
        New(defn.vcArrayOf(cls).typeRef, List(underlyingArray, ref(mod)))
      case _ =>
        tree match {
          case SeqLiteral(elems, tptr) =>
            cpy.SeqLiteral(tree)(elems, TypeTree(transformTypeOfTree(tptr)))
          case _ => tree
        }
    }

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)

  override def transformTypeTree(tree: TypeTree)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)

  override def transformBlock(tree: Block)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)

  override def transformIf(tree: If)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)

  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    def isArrayWithEVT(tpe: Any): Option[ErasedValueType] =
      tpe match {
        case JavaArrayType(evt@ErasedValueType(tr, underlying)) => Some(evt)
        case JavaArrayType(ijat: JavaArrayType) => isArrayWithEVT(ijat)
        case _ => None
      }
    tree match {
      // newArray(args) => New VCXArray(newXArray(args'), V)
      case ap@Apply(fun, List(compTpt, retTpt, dims @SeqLiteral(elems, elemtpt)))
          if (fun.symbol == defn.newArrayMethod) =>
        val Literal(Constant(ins)) = retTpt
        ins match {
          case JavaArrayType(ErasedValueType(tr, underlying)) =>
            val cls = tr.symbol.asClass
            val mod = cls.companionModule
            val arTpe = JavaArrayType(underlying)
            New(defn.vcArrayOf(cls).typeRef,
              List(newArray(underlying, arTpe, tree.pos, dims.asInstanceOf[JavaSeqLiteral]).ensureConforms(arTpe),
                ref(mod)))
          case _: Type =>
            val evtOpt = isArrayWithEVT(ins)
            evtOpt match {
              case Some(evt1@ErasedValueType(tr1, und1)) =>
                val cls = tr1.symbol.asClass
                val mod = cls.companionModule
                val retTpt2 = eraseVCArrays(ins.asInstanceOf[Type])
                val Literal(Constant(compTptType)) = compTpt
                val compTpt2 = eraseVCArrays((compTptType.asInstanceOf[Type]))
                newArray(compTpt2, retTpt2, tree.pos, dims.asInstanceOf[JavaSeqLiteral])
              case _ => tree
            }
        }
      // array.[]update(idx, elem) => array.arr().[]update(idx, elem)
      case Apply(Select(array, nme.primitive.arrayUpdate), List(idx, elem)) =>
        elem.tpe.widen match {
          case ErasedValueType(tr, _) =>
            val cls = tr.symbol.asClass
            array.select(nme.ARR).appliedToNone
              .select(nme.primitive.arrayUpdate).appliedTo(idx, ref(evt2u(cls)).appliedTo(elem))
          case _ =>
            tree
        }
      // array.[]apply(idx) => array.arr().[]apply(idx)
      case t@Apply(Select(array, nme.primitive.arrayApply), List(idx)) =>
        tree.tpe.widen match {
          case ErasedValueType(tr, undType) =>
            val cls = tr.symbol.asClass
            if (undType.classSymbol.isPrimitiveValueClass)
              ref(u2evt(cls)).appliedTo(array.select(nme.ARR).appliedToNone
                .select(nme.primitive.arrayApply).appliedTo(idx))
            else
              ref(u2evt(cls)).appliedTo(array.select(nme.ARR).appliedToNone
                .select(nme.primitive.arrayApply).appliedTo(idx).ensureConforms(undType))
          case _ =>
            transformTypeOfTree(tree)
        }
      // array.[]length() => array.arr().[]length()
      case Apply(Select(array, nme.primitive.arrayLength), Nil)
          if (array.tpe <:< defn.VCArrayPrototypeType) =>
        array.select(nme.ARR).appliedToNone
          .select(nme.primitive.arrayLength).appliedToNone
      case _ =>
        transformTypeOfTree(tree)
    }
  }
}