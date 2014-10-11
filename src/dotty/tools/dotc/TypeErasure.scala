package dotty.tools
package dotc
package core

import Symbols._, Types._, Contexts._, Flags._, Names._, StdNames._, Decorators._, Flags.JavaDefined
import dotc.transform.ExplicitOuter._
import util.DotClass

/** Erased types are:
 *
 *  TypeRefWithFixedSym(denot is ClassDenotation)
 *  TermRefWithFixedSym(denot is SymDenotation)
 *  JavaArrayType
 *  AnnotatedType
 *  MethodType
 *  ThisType
 *  SuperType
 *  ClassInfo (NoPrefix, ...)
 *  NoType
 *  NoPrefix
 *  WildcardType
 *  ErrorType
 *
 *  only for isInstanceOf, asInstanceOf: PolyType, PolyParam, TypeBounds
 *
 */
object TypeErasure {

  /** A predicate that tests whether a type is a legal erased type. Only asInstanceOf and
   *  isInstanceOf may have types that do not satisfy the predicate.
   */
  def isErasedType(tp: Type)(implicit ctx: Context): Boolean = tp match {
    case tp: TypeRef =>
      tp.symbol.isClass
    case _: TermRef =>
      true
    case JavaArrayType(elem) =>
      isErasedType(elem)
    case AnnotatedType(_, tp) =>
      isErasedType(tp)
    case ThisType(tref) =>
      isErasedType(tref)
    case tp: MethodType =>
      tp.paramTypes.forall(isErasedType) && isErasedType(tp.resultType)
    case tp @ ClassInfo(pre, _, parents, decls, _) =>
      isErasedType(pre) && parents.forall(isErasedType) //&& decls.forall(sym => isErasedType(sym.info)) && isErasedType(tp.selfType)
    case NoType | NoPrefix | WildcardType | ErrorType | SuperType(_, _) =>
      true
    case _ =>
      false
  }

  case class ErasedValueType(cls: ClassSymbol, underlying: Type) extends CachedGroundType {
    override def computeHash = doHash(cls, underlying)
  }

  private def erasureIdx(isJava: Boolean, isSemi: Boolean, isConstructor: Boolean, wildcardOK: Boolean) =
    (if (isJava) 1 else 0) +
    (if (isSemi) 2 else 0) +
    (if (isConstructor) 4 else 0) +
    (if (wildcardOK) 8 else 0)

  private val erasures = new Array[TypeErasure](16)

  for {
    isJava <- List(false, true)
    isSemi <- List(false, true)
    isConstructor <- List(false, true)
    wildcardOK <- List(false, true)
  } erasures(erasureIdx(isJava, isSemi, isConstructor, wildcardOK)) =
    new TypeErasure(isJava, isSemi, isConstructor, wildcardOK)

  /** Produces an erasure function.
   *  @param isJava        Arguments should be treated the way Java does it
   *  @param isSemi        Value classes are mapped in an intermediate step to
   *                       ErasedValueClass types, instead of going directly to
   *                       the erasure of the underlying type.
   *  @param isConstructor Argument forms part of the type of a constructor
   *  @param wildcardOK    Wildcards are acceptable (true when using the erasure
   *                       for computing a signature name).
   */
  private def erasureFn(isJava: Boolean, isSemi: Boolean, isConstructor: Boolean, wildcardOK: Boolean): TypeErasure =
    erasures(erasureIdx(isJava, isSemi, isConstructor, wildcardOK))

  private val scalaErasureFn = erasureFn(isJava = false, isSemi = false, isConstructor = false, wildcardOK = false)
  private val scalaSigFn = erasureFn(isJava = false, isSemi = false, isConstructor = false, wildcardOK = true)
  private val javaSigFn = erasureFn(isJava = true, isSemi = false, isConstructor = false, wildcardOK = true)
  private val semiErasureFn = erasureFn(isJava = false, isSemi = true, isConstructor = false, wildcardOK = false)

  def erasure(tp: Type)(implicit ctx: Context): Type = scalaErasureFn(tp)
  def semiErasure(tp: Type)(implicit ctx: Context): Type = semiErasureFn(tp)
  def sigName(tp: Type, isJava: Boolean)(implicit ctx: Context): TypeName = {
    val normTp =
      if (tp.isRepeatedParam) tp.translateParameterized(defn.RepeatedParamClass, defn.SeqClass)
      else tp
    (if (isJava) javaSigFn else scalaSigFn).sigName(normTp)
  }

  /**  The symbol's erased info. This is the type's erasure, except for the following symbols:
   *
   *   - For $asInstanceOf           : [T]T
   *   - For $isInstanceOf           : [T]Boolean
   *   - For all abstract types      : = ?
   *   - For all other symbols       : the semi-erasure of their types, with
   *                                   isJava, isConstructor set according to symbol.
   */
  def transformInfo(sym: Symbol, tp: Type)(implicit ctx: Context): Type = {
    val erase = erasureFn(sym is JavaDefined, isSemi = true, sym.isConstructor, wildcardOK = false)

    def eraseParamBounds(tp: PolyType): Type =
      tp.derivedPolyType(
        tp.paramNames, tp.paramNames map (Function.const(TypeBounds.upper(defn.ObjectType))), tp.resultType)

    if ((sym eq defn.Any_asInstanceOf) || (sym eq defn.Any_isInstanceOf)) eraseParamBounds(sym.info.asInstanceOf[PolyType])
    else if (sym.isAbstractType) TypeAlias(WildcardType)
    else if (sym.isConstructor) outer.addParam(sym.owner.asClass, erase(tp))
    else erase(tp)
  }

  def isUnboundedGeneric(tp: Type)(implicit ctx: Context) = !(
    (tp derivesFrom defn.ObjectClass) ||
    tp.classSymbol.isPrimitiveValueClass ||
    (tp.typeSymbol is JavaDefined))

  /** The erased least upper bound is computed as follows
   *  - if both argument are arrays, an array of the lub of the element types
   *  - if one argument is an array, Object
   *  - otherwise a common superclass or trait S of the argument classes, with the
   *    following two properties:
   *      S is minimal: no other common superclass or trait derives from S]
   *      S is last   : in the linearization of the first argument type `tp1`
   *                    there are no minimal common superclasses or traits that
   *                    come after S.
   *  (the reason to pick last is that we prefer classes over traits that way).
   */
  def erasedLub(tp1: Type, tp2: Type)(implicit ctx: Context): Type = tp1 match {
    case JavaArrayType(elem1) =>
      tp2 match {
        case JavaArrayType(elem2) => JavaArrayType(erasedLub(elem1, elem2))
        case _ => defn.ObjectType
      }
    case _ =>
      tp2 match {
        case JavaArrayType(_) => defn.ObjectType
        case _ =>
          val cls2 = tp2.classSymbol
          def loop(bcs: List[ClassSymbol], bestSoFar: ClassSymbol): ClassSymbol = bcs match {
            case bc :: bcs1 =>
              if (cls2.derivesFrom(bc))
                if (!bc.is(Trait)) bc
                else loop(bcs1, if (bestSoFar.derivesFrom(bc)) bestSoFar else bc)
              else
                loop(bcs1, bestSoFar)
            case nil =>
              bestSoFar
          }
          loop(tp1.baseClasses, defn.ObjectClass).typeRef
      }
  }

  /** The erased greatest lower bound picks one of the two argument types. It prefers, in this order:
   *  - arrays over non-arrays
   *  - subtypes over supertypes, unless isJava is set
   *  - real classes over traits
   */
  def erasedGlb(tp1: Type, tp2: Type, isJava: Boolean)(implicit ctx: Context): Type = tp1 match {
    case JavaArrayType(elem1) =>
      tp2 match {
        case JavaArrayType(elem2) => JavaArrayType(erasedGlb(elem1, elem2, isJava))
        case _ => tp1
      }
    case _ =>
      tp2 match {
        case JavaArrayType(_) => tp2
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
}
import TypeErasure._

/**
 *  This is used as the Scala erasure during the erasure phase itself
   *  It differs from normal erasure in that value classes are erased to ErasedValueTypes which
   *  are then later converted to the underlying parameter type in phase posterasure.
 *
 */
class TypeErasure(isJava: Boolean, isSemi: Boolean, isConstructor: Boolean, wildcardOK: Boolean) extends DotClass {

  /**  The erasure |T| of a type T. This is:
   *
   *   - For a refined type scala.Array+[T]:
   *      - if T is Nothing or Null, []Object
   *      - otherwise, if T <: Object, []|T|
   *      - otherwise, if T is a type paramter coming from Java, []Object
   *      - otherwise, Object
   *   - For a term ref p.x, the type <noprefix> # x.
   *   - For a typeref scala.Any, scala.AnyVal, scala.Singleon or scala.NotNull: |java.lang.Object|
   *   - For a typeref scala.Unit, |scala.runtime.BoxedUnit|.
   *   - For a typeref P.C where C refers to a class, <noprefix> # C.
   *   - For a typeref P.C where C refers to an alias type, the erasure of C's alias.
   *   - For a typeref P.C where C refers to an abstract type, the erasure of C's upper bound.
   *   - For a this-type C.this, the type itself.
   *   - For all other type proxies: The erasure of the underlying type.
   *   - For T1 & T2, the erased glb of |T1| and |T2| (see erasedGlb)
   *   - For T1 | T2, the first base class in the linearization of T which is also a base class of T2
   *   - For => T, ()T
   *   - For a method type (Fs)scala.Unit, (|Fs|)scala.Unit.
   *   - For any other uncurried method type (Fs)T, (|Fs|)|T|.
   *   - For a curried method type (Fs1)(Fs2)T, (|Fs1|,Es2)ET where (Es2)ET = |(Fs2)T|.
   *   - For a polymorphic type [Ts](Ps)T, |(Ps)T|
   *   _ For a polymorphic type [Ts]T where T is not a method type, ()|T|
   *   - For the class info type of java.lang.Object, the same type without any parents.
   *   - For a class info type of a value class, the same type without any parents.
   *   - For any other class info type with parents Ps, the same type with
   *     parents |Ps|, but with duplicate references of Object removed.
   *   - For NoType or NoPrefix, the type itself.
   *   - For any other type, exception.
   */
  def apply(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: TypeRef =>
      val sym = tp.symbol
      if (!sym.isClass) this(tp.info)
      else if (sym.isDerivedValueClass) eraseDerivedValueClassRef(tp)
      else eraseNormalClassRef(tp)
    case tp: RefinedType =>
      val parent = tp.parent
      if (parent isRef defn.ArrayClass) eraseArray(tp)
      else this(parent)
    case tp: TermRef =>
      assert(tp.symbol.exists, tp)
      TermRef(this(tp.prefix), tp.symbol.asTerm)
    case ThisType(_) | SuperType(_, _) =>
      tp
    case ExprType(rt) =>
      MethodType(Nil, Nil, this(rt))
    case tp: TypeProxy =>
      this(tp.underlying)
    case AndType(tp1, tp2) =>
      erasedGlb(this(tp1), this(tp2), isJava)
    case OrType(tp1, tp2) =>
      ctx.typeComparer.orType(this(tp1), this(tp2), erased = true)
    case tp: MethodType =>
      val paramErasure = erasureFn(tp.isJava, isSemi, isConstructor, wildcardOK)(_)
      val formals = tp.paramTypes.mapConserve(paramErasure)
      eraseResult(tp.resultType) match {
        case rt: MethodType =>
          tp.derivedMethodType(tp.paramNames ++ rt.paramNames, formals ++ rt.paramTypes, rt.resultType)
        case rt =>
          tp.derivedMethodType(tp.paramNames, formals, rt)
      }
    case tp: PolyType =>
      this(tp.resultType) match {
        case rt: MethodType => rt
        case rt => MethodType(Nil, Nil, rt)
      }
    case tp @ ClassInfo(pre, cls, classParents, decls, _) =>
      if (cls is Package) tp
      else {
        def eraseTypeRef(p: TypeRef) = this(p).asInstanceOf[TypeRef]
        val parents: List[TypeRef] =
          if ((cls eq defn.ObjectClass) || cls.isPrimitiveValueClass) Nil
          else removeLaterObjects(classParents.mapConserve(eraseTypeRef))
        val erasedDecls = decls.filteredScope(d => !d.isType || d.isClass)
        tp.derivedClassInfo(NoPrefix, parents, erasedDecls, this(tp.selfType))
          // can't replace selftype by NoType because this would lose the sourceModule link
      }
    case NoType | NoPrefix | ErrorType | JavaArrayType(_) =>
      tp
    case tp: WildcardType if wildcardOK =>
      tp
  }

  def eraseArray(tp: RefinedType)(implicit ctx: Context) = {
    val defn.ArrayType(elemtp) = tp
    if (elemtp derivesFrom defn.NullClass) JavaArrayType(defn.ObjectType)
    else if (isUnboundedGeneric(elemtp))
      elemtp match {
        case elemtp: TypeRef if elemtp.symbol.is(JavaDefined) => JavaArrayType(defn.ObjectType)
        case _ => defn.ObjectType
      }
    else JavaArrayType(this(elemtp))
  }

  private def eraseDerivedValueClassRef(tref: TypeRef)(implicit ctx: Context): Type =
    unsupported("eraseDerivedValueClass")

  private def eraseNormalClassRef(tref: TypeRef)(implicit ctx: Context): Type = {
    val cls = tref.symbol.asClass
    (if (cls.owner is Package) normalizeClass(cls) else cls).typeRef
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

  private def removeLaterObjects(trs: List[TypeRef])(implicit ctx: Context): List[TypeRef] = trs match {
    case tr :: trs1 => tr :: trs1.filterNot(_ isRef defn.ObjectClass)
    case nil => nil
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
      sigName(this(tp))
    case JavaArrayType(elem) =>
      sigName(elem) ++ "[]"
    case tp: TypeBounds =>
      sigName(tp.hi)
    case ExprType(rt) =>
      sigName(defn.FunctionType(Nil, rt))
    case tp: TypeProxy =>
      sigName(tp.underlying)
    case ErrorType | WildcardType =>
      tpnme.WILDCARD
    case tp: WildcardType =>
      sigName(tp.optBounds)
    case _ =>
      val erased = this(tp)
      assert(erased ne tp, tp)
      sigName(erased)
  }
}
