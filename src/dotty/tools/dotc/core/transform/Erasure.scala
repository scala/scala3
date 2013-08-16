package dotty.tools.dotc
package core
package transform

import Symbols._, Types._, Contexts._, Flags._, Names._, StdNames._

object Erasure {

  /**  The erasure |T| of a type T. This is:
   *
   *   - For a refined type scala.Array+[T]:
   *      - if T is Nothing or Null, scala.Array+[Object]
   *      - otherwise, if T <: Object, scala.Array+[|T|]
   *      - otherwise, if T is a type paramter coming from Java, scala.Array+[Object].
   *      - otherwise, Object
   *   - For a constant type, NoType or NoPrefix, the type itself.
   *   - For all other type proxies: The erasure of the underlying type.
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
   *   - For any other type, exception.
   */
  def erasure(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: TypeRef =>
      val sym = tp.symbol
      if (sym.isClass)
        /*if (sym.isDerivedValueClass) eraseDerivedValueClassRef(tref)
        else */if (sym.owner is Package) normalizeClass(sym.asClass).typeConstructor
        else tp.derivedNamedType(erasure(tp.prefix))
      else erasure(sym.info)
    case tp: RefinedType =>
      val parent = tp.parent
      if (parent.isArray) eraseArray(tp)
      else erasure(parent)
    case ConstantType(_) | NoType | NoPrefix =>
      tp
    case tp: TypeProxy =>
      erasure(tp.underlying)
    case AndType(tp1, tp2) =>
      erasure(tp1)
    case OrType(tp1, tp2) =>
      erasure(tp.baseType(lubClass(tp1, tp2)))
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
    case ErrorType =>
      tp
  }

  def eraseArray(tp: RefinedType)(implicit ctx: Context) = {
    val (n, elemtp) = tp.splitArray
    val elemCls = elemtp.classSymbol
    if (elemCls.isSubClass(defn.NullClass))
      defn.ObjectArrayType
    else if (elemCls.isSubClass(defn.ObjectClass) || elemCls.isPrimitiveValueClass)
      (erasure(elemtp) /: (0 until n))((erased, _) =>
        defn.ArrayType.appliedTo(erased))
    else if (elemtp.typeSymbol is JavaDefined)
      defn.ObjectArrayType
    else
      defn.ObjectType
  }

  def normalizeClass(cls: ClassSymbol)(implicit ctx: Context): ClassSymbol = {
    if (cls.owner == defn.ScalaPackageClass) {
      if (cls == defn.AnyClass || cls == defn.AnyValClass || cls == defn.SingletonClass || cls == defn.NotNullClass)
        return defn.ObjectClass
      if (cls == defn.UnitClass)
        return defn.BoxedUnitClass
    }
    cls
  }

  def lubClass(tp1: Type, tp2: Type)(implicit ctx: Context): ClassSymbol = {
    var bcs1 = tp1.baseClasses
    val bc2 = tp2.baseClasses.head
    while (bcs1.nonEmpty && !bc2.derivesFrom(bcs1.head))
      bcs1 = bcs1.tail
    if (bcs1.isEmpty) defn.ObjectClass else bcs1.head
  }

  /** The parameter signature of a type.
   *  Need to ensure correspondence with erasure
   */
  def paramSignature(tp: Type)(implicit ctx: Context): TypeName = tp match {
    case tp: TypeRef =>
      val sym = tp.symbol
      if (sym.isClass)
        /*if (sym.isDerivedValueClass) eraseDerivedValueClassRef(tref)
        else */if (sym.owner is Package) normalizeClass(sym.asClass).name
        else sym.asClass.name
      else paramSignature(sym.info)
    case tp: RefinedType =>
      val parent = tp.parent
      if (parent.isArray)
        eraseArray(tp) match {
          case tp1: RefinedType if tp1.parent.isArray =>
            paramSignature(tp1.refinedInfo) ++ "[]"
          case tp1 =>
            paramSignature(tp1)
        }
      else paramSignature(parent)
    case tp: TypeProxy =>
      paramSignature(tp.underlying)
    case AndType(tp1, tp2) =>
      paramSignature(tp1)
    case OrType(tp1, tp2) =>
      lubClass(tp1, tp2).name
    case ErrorType =>
      tpnme.WILDCARD
  }

  def resultErasure(tp: Type)(implicit ctx: Context) =
    if (tp.isClassType(defn.UnitClass)) tp else erasure(tp)

  def removeLaterObjects(trs: List[TypeRef])(implicit ctx: Context): List[TypeRef] = trs match {
    case tr :: trs1 => tr :: trs1.filterNot(_.isClassType(defn.ObjectClass))
    case nil => nil
  }
}
