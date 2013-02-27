package dotty.tools.dotc
package core
package transform

import Flags._
import Symbols._, Types._, Contexts._

object Erasure {

  /**  The erasure |T| of a type T. This is: !!! todo: update
   *
   *   - For a constant type, itself.
   *   - For a refined type scala.Array+[T]:
   *      - if T is Nothing or Null, scala.Array+[Object]
   *      - otherwise, if T <: Object, scala.Array+[|T|]
   *      - otherwise, if T is a type paramter coming from Java, scala.Array+[Object].
   *      - otherwise, Object
   *   - For a typeref scala.Any, scala.AnyVal, scala.Singleon or scala.NotNull, java.lang.Object.
   *   - For a typeref scala.Unit, scala.runtime.BoxedUnit.
   *   - For a typeref P.C where C refers to a toplevel class, P.C.
   *   - For a typeref P.C where C refers to a nested class, |P|.C.
   *   - For a typeref P.C where C refers to an alias type, the erasure of C's alias.
   *   - For a typeref P.C where C refers to an abstract type, the erasure of C's upper bound.
   *   - For T1 & T2, erasure(T1)  (??)
   *   - For T1 | T2, the first base class in the linearization of T which is also a base class of T2
   *   - For a method type (Fs)scala.Unit, (|Fs|)scala.Unit.
   *   - For any other method type (Fs)T, (|Fs|)|T|.
   *   - For a polymorphic type, the erasure of its result type.
   *   - For the class info type of java.lang.Object, the same type without any parents.
   *   - For a class info type of a value class, the same type without any parents.
   *   - For any other class info type with parents Ps, the same type with
   *     parents |Ps|, but with duplicate references of Object removed.
   *   - for all other types, the type itself (with any sub-components erased)
   */
  def erasure(tp: Type)(implicit ctx: Context): Type = tp match {
    case ConstantType(_) =>
      tp
    case tp: RefinedType =>
      val parent = tp.parent
      if (parent.dealias.typeSymbol == defn.ArrayClass) {
        val (n, elemtp) = tp.splitArray
        if (elemtp <:< defn.NullType)
          defn.ObjectArrayType
        else if (elemtp <:< defn.ObjectType)
          (erasure(elemtp) /: (0 until n))((erased, _) =>
            defn.ArrayType.appliedTo(erased))
        else if (elemtp.typeSymbol is JavaDefined)
          defn.ObjectArrayType
        else
          defn.ObjectType
      } else erasure(parent)
    case tp: TypeRef =>
      val sym = tp.symbol
      if (sym == defn.AnyClass || sym == defn.AnyValClass || sym == defn.SingletonClass || sym == defn.NotNullClass)
        defn.ObjectType
      else if (sym == defn.UnitClass) defn.BoxedUnitClass.typeConstructor
      //      else if (sym.isDerivedValueClass) eraseDerivedValueClassRef(tref)
      else if (sym.isClass)
        if (sym.owner.isPackage) tp
        else tp.derivedNamedType(erasure(tp.prefix))
      else
        erasure(sym.info)
    case NoType | NoPrefix =>
      tp
    case tp: TypeProxy =>
      erasure(tp.underlying)
    case AndType(tp1, tp2) =>
      erasure(tp1)
    case OrType(tp1, tp2) =>
      var bcs1 = tp1.baseClasses
      val bc2 = tp2.baseClasses.head
      while (bcs1.nonEmpty && !bc2.isNonBottomSubClass(bcs1.head))
        bcs1 = bcs1.tail
      if (bcs1.isEmpty) defn.ObjectType
      else erasure(bcs1.head.typeConstructor)
    case tp: MethodType =>
      tp.derivedMethodType(
        tp.paramNames, tp.paramTypes.mapConserve(erasure), resultErasure(tp.resultType))
    case tp: PolyType =>
      erasure(tp.resultType)
    case tp @ ClassInfo(pre, cls, classParents, decls, optSelfType) =>
      val parents: List[TypeRef] =
        if (cls == defn.ObjectClass || cls.isPrimitiveValueClass) Nil
        else if (cls == defn.ArrayClass) defn.ObjectClass.typeConstructor :: Nil
        else removeLaterObjects(classParents mapConserve (erasure(_).asInstanceOf[TypeRef]))
      tp.derivedClassInfo(erasure(pre), parents, NoType)
  }

  def resultErasure(tp: Type)(implicit ctx: Context) =
    if (tp.typeSymbol == defn.UnitClass) tp else erasure(tp)

  def removeLaterObjects(trs: List[TypeRef])(implicit ctx: Context): List[TypeRef] = trs match {
    case tr :: trs1 => tr :: (trs1 filter (_.typeSymbol != defn.ObjectClass))
    case nil => nil
  }
}
