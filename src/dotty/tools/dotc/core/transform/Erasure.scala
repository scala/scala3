package dotty.tools.dotc
package core
package transform

import Symbols._, Types._, Contexts._, Flags._, Names._, StdNames._, Flags.JavaDefined
import util.DotClass

object Erasure {

  case class ErasedValueType(cls: ClassSymbol, underlying: Type) extends CachedGroundType {
    override def computeHash = doHash(cls, underlying)
  }

  private def erasureIdx(isJava: Boolean, isSemi: Boolean, isConstructor: Boolean, wildcardOK: Boolean) =
    (if (isJava) 1 else 0) +
    (if (isSemi) 2 else 0) +
    (if (isConstructor) 4 else 0) +
    (if (wildcardOK) 8 else 0)

  private var erasures = new Array[Erasure](16)

  for {
    isJava <- List(false, true)
    isSemi <- List(false, true)
    isConstructor <- List(false, true)
    wildcardOK <- List(false, true)
  } erasures(erasureIdx(isJava, isSemi, isConstructor, wildcardOK)) =
    new Erasure(isJava, isSemi, isConstructor, wildcardOK)

  /** Produces an erasure function.
   *  @param isJava        Arguments should be treated the way Java does it
   *  @param isSemi        Value classes are mapped in an intermediate step to
   *                       ErasedValueClass types, instead of going directly to
   *                       the erasure of the underlying type.
   *  @param isConstructor Argument forms part of the type of a constructor
   *  @param wildcardOK    Wildcards are acceptable (true when using the erasure
   *                       for computing a signature name).
   */
  private def erasureFn(isJava: Boolean, isSemi: Boolean, isConstructor: Boolean, wildcardOK: Boolean): Erasure =
    erasures(erasureIdx(isJava, isSemi, isConstructor, wildcardOK))

  private val scalaErasureFn = erasureFn(isJava = false, isSemi = false, isConstructor = false, wildcardOK = false)
  private val scalaSigFn = erasureFn(isJava = false, isSemi = false, isConstructor = false, wildcardOK = true)
  private val javaSigFn = erasureFn(isJava = true, isSemi = false, isConstructor = false, wildcardOK = true)
  private val semiErasureFn = erasureFn(isJava = false, isSemi = true, isConstructor = false, wildcardOK = false)

  def erasure(tp: Type)(implicit ctx: Context): Type = scalaErasureFn(tp)
  def semiErasure(tp: Type)(implicit ctx: Context): Type = semiErasureFn(tp)
  def sigName(tp: Type, isJava: Boolean)(implicit ctx: Context): TypeName =
    (if (isJava) javaSigFn else scalaSigFn).sigName(tp)

  /**  The symbol's erased info. This is the type's erasure, except for the following symbols:
   *
   *   - For $asInstanceOf           : [T]T
   *   - For $isInstanceOf           : [T]scala#Boolean
   *   - For Array[T].<init>         : [T]{scala#Int)Array[T]
   *   - For type members of Array   : The original info
   *   - For all other abstract types: = ?
   *   - For all other symbols       : the semi-erasure of their types, with
   *                                   isJava, isConstructor set according to symbol.
   */
  def transformInfo(sym: Symbol, tp: Type)(implicit ctx: Context): Type = {
    val erase = erasureFn(sym is JavaDefined, isSemi = true, sym.isConstructor, wildcardOK = false)
    if ((sym eq defn.Object_asInstanceOf) ||
        (sym eq defn.Object_isInstanceOf) ||
        (sym.owner eq defn.ArrayClass) && (sym.isType || sym.isConstructor)) sym.info
    else if (sym.isAbstractType) TypeAlias(WildcardType)
    else erase(tp)
  }

  def isUnboundedGeneric(tp: Type)(implicit ctx: Context) = !(
    (tp derivesFrom defn.ObjectClass) ||
    tp.classSymbol.isPrimitiveValueClass ||
    (tp.typeSymbol is JavaDefined))

}
import Erasure._

/**
 *  This is used as the Scala erasure during the erasure phase itself
   *  It differs from normal erasure in that value classes are erased to ErasedValueTypes which
   *  are then later converted to the underlying parameter type in phase posterasure.
 *
 */
class Erasure(isJava: Boolean, isSemi: Boolean, isConstructor: Boolean, wildcardOK: Boolean) extends DotClass {

  /**  The erasure |T| of a type T. This is:
   *
   *   - For a refined type scala.Array+[T]:
   *      - if T is Nothing or Null, scala.Array+[Object]
   *      - otherwise, if T <: Object, scala.Array+[|T|]
   *      - otherwise, if T is a type paramter coming from Java, scala.Array+[Object].
   *      - otherwise, Object
   *   - For a constant type, NoType or NoPrefix, the type itself.
   *   - For all other type proxies: The erasure of the underlying type.
   *   - For a typeref scala.Any, scala.AnyVal, scala.Singleon or scala.NotNull: java.lang.Object.
   *   - For a typeref scala.Unit, scala.runtime.BoxedUnit.
   *   - For a typeref whose symbol is owned by Array: The typeref itself
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
  def apply(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: TypeRef =>
      val sym = tp.symbol
      if (!sym.isClass)
        if (sym.exists && (sym.owner eq defn.ArrayClass)) tp else this(tp.info) //!!!!
      else if (sym.isDerivedValueClass) eraseDerivedValueClassRef(tp)
      else eraseNormalClassRef(tp)
    case tp: RefinedType =>
      val parent = tp.parent
      if (parent isRef defn.ArrayClass) eraseArray(tp)
      else this(parent)
    case tp: TermRef =>
      val sym = tp.symbol
      if (sym.owner is Package) sym.termRef
      else tp.derivedSelect(this(tp.prefix))
    case _: ThisType | _: ConstantType =>
      tp
    case tp: TypeProxy =>
      this(tp.underlying)
    case AndType(tp1, tp2) =>
      mergeAnd(this(tp1), this(tp2))
    case OrType(tp1, tp2) =>
      this(tp.baseTypeRef(lubClass(tp1, tp2)))
    case tp: MethodType =>
      val paramErasure = erasureFn(tp.isJava, isSemi, isConstructor, wildcardOK)(_)
      tp.derivedMethodType(
        tp.paramNames, tp.paramTypes.mapConserve(paramErasure), eraseResult(tp.resultType))
    case tp: PolyType =>
      this(tp.resultType)
    case tp @ ClassInfo(pre, cls, classParents, decls, _) =>
      def eraseTypeRef(p: TypeRef) = this(p).asInstanceOf[TypeRef]
      val parents: List[TypeRef] =
        if ((cls eq defn.ObjectClass) || cls.isPrimitiveValueClass) Nil
        else if (cls eq defn.ArrayClass) defn.ObjectClass.typeRef :: Nil
        else removeLaterObjects(classParents.mapConserve(eraseTypeRef))
      tp.derivedClassInfo(this(pre), parents, this(tp.selfType))
    case NoType | NoPrefix | ErrorType =>
      tp
    case tp: WildcardType if wildcardOK =>
      tp
  }

  private def eraseArray(tp: RefinedType)(implicit ctx: Context) = {
    val defn.ArrayType(elemtp) = tp
    if (elemtp derivesFrom defn.NullClass) defn.ObjectArrayType
    else if (isUnboundedGeneric(elemtp)) defn.ObjectType
    else defn.ArrayType(this(elemtp))
  }

  private def eraseDerivedValueClassRef(tref: TypeRef)(implicit ctx: Context): Type =
    unsupported("eraseDerivedValueClass")

  private def eraseNormalClassRef(tref: TypeRef)(implicit ctx: Context): Type = {
    val sym = tref.symbol
    if (sym.owner is Package) normalizeClass(sym.asClass).typeRef
    else tref.derivedSelect(this(tref.prefix))
  }

  private def eraseResult(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: TypeRef =>
      val sym = tp.typeSymbol
      if (sym eq defn.UnitClass) sym.typeRef
      else if (sym.isDerivedValueClass) eraseNormalClassRef(tp)
      else this(tp)
    case RefinedType(parent, _) if !(parent isRef defn.ArrayClass) =>
      eraseResult(parent)
    case _ =>
      this(tp)
  }

  private def normalizeClass(cls: ClassSymbol)(implicit ctx: Context): ClassSymbol = {
    if (cls.owner == defn.ScalaPackageClass) {
      if (cls == defn.AnyClass || cls == defn.AnyValClass || cls == defn.SingletonClass || cls == defn.NotNullClass)
        return defn.ObjectClass
      if (cls == defn.UnitClass)
        return defn.BoxedUnitClass
    }
    cls
  }

  private def lubClass(tp1: Type, tp2: Type)(implicit ctx: Context): ClassSymbol = {
    var bcs1 = tp1.baseClasses
    val bc2 = tp2.baseClasses.head
    while (bcs1.nonEmpty && !bc2.derivesFrom(bcs1.head))
      bcs1 = bcs1.tail
    if (bcs1.isEmpty) defn.ObjectClass else bcs1.head
  }

  private def removeLaterObjects(trs: List[TypeRef])(implicit ctx: Context): List[TypeRef] = trs match {
    case tr :: trs1 => tr :: trs1.filterNot(_ isRef defn.ObjectClass)
    case nil => nil
  }

  private def mergeAnd(tp1: Type, tp2: Type)(implicit ctx: Context): Type = tp1 match {
    case defn.ArrayType(elem1) =>
      tp2 match {
        case defn.ArrayType(elem2) => defn.ArrayType(mergeAnd(elem1, elem2))
        case _ => defn.ObjectType
      }
    case _ =>
      tp2 match {
        case defn.ArrayType(_) => defn.ObjectType
        case _ =>
          val tsym1 = tp1.typeSymbol
          val tsym2 = tp2.typeSymbol
          if (!tsym2.exists) tp1
          else if (!tsym1.exists) tp2
          else if (!isJava && tsym1.derivesFrom(tsym2)) tp1
          else if (!isJava && tsym2.derivesFrom(tsym1)) tp2
          else if (tp1.typeSymbol.isRealClass) tp1
          else if (tp2.typeSymbol.isRealClass) tp2
          else tp1
      }
  }

  /** The name of the type as it is used in `Signature`s.
   *  Need to ensure correspondence with erasure!
   */
  def sigName(tp: Type)(implicit ctx: Context): TypeName = tp match {
    case tp: TypeRef =>
      val sym = tp.symbol
      if (!sym.isClass) sigName(tp.info)
      else if (sym.isDerivedValueClass) sigName(eraseDerivedValueClassRef(tp))
      else normalizeClass(sym.asClass).fullName.asTypeName
    case defn.ArrayType(elem) =>
      sigName(elem) ++ "[]"
    case tp: TypeBounds =>
      sigName(tp.hi)
    case tp: TypeProxy =>
      sigName(tp.underlying)
    case ErrorType | WildcardType =>
      tpnme.WILDCARD
    case tp: WildcardType =>
      sigName(tp.optBounds)
    case _ =>
      val erased = this(tp)
      assert(erased ne tp)
      sigName(erased)
  }
}
