package dotty.tools
package dotc
package core

import Symbols._, Types._, Contexts._, Flags._, Names._, StdNames._, Decorators._, Flags.JavaDefined
import dotc.transform.ExplicitOuter._
import typer.Mode
import util.DotClass

/** Erased types are:
 *
 *  TypeRef(prefix is ignored, denot is ClassDenotation)
 *  TermRef(prefix is ignored, denot is SymDenotation)
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
      tp.symbol.isClass && tp.symbol != defn.AnyClass
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

  /** The current context with a phase no later than erasure */
  private def erasureCtx(implicit ctx: Context) =
    if (ctx.erasedTypes) ctx.withPhase(ctx.erasurePhase).addMode(Mode.FutureDefsOK) else ctx

  def erasure(tp: Type)(implicit ctx: Context): Type = scalaErasureFn(tp)(erasureCtx)
  def semiErasure(tp: Type)(implicit ctx: Context): Type = semiErasureFn(tp)(erasureCtx)
  def sigName(tp: Type, isJava: Boolean)(implicit ctx: Context): TypeName = {
    val seqClass = if (isJava) defn.ArrayClass else defn.SeqClass
    val normTp =
      if (tp.isRepeatedParam) tp.translateParameterized(defn.RepeatedParamClass, seqClass)
      else tp
    (if (isJava) javaSigFn else scalaSigFn).sigName(normTp)(erasureCtx)
  }

  /** The erasure of a top-level reference. Differs from normal erasure in that
   *  TermRefs are kept instead of being widened away.
   */
  def erasedRef(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: TermRef =>
      assert(tp.symbol.exists, tp)
      val tp1 = ctx.makePackageObjPrefixExplicit(tp)
      if (tp1 ne tp) erasedRef(tp1)
      else TermRef(erasedRef(tp.prefix), tp.symbol.asTerm)
    case tp: ThisType =>
      tp
    case tp =>
      erasure(tp)
  }

  /** The erasure of a symbol's info. This is different of `erasure` in the way `ExprType`s are
   *  treated. `eraseInfo` maps them them to nullary method types, whereas `erasure` maps them
   *  to `Function0`.
   */
  def eraseInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type =
    scalaErasureFn.eraseInfo(tp, sym)(erasureCtx)

  /** The erasure of a function result type. Differs from normal erasure in that
   *  Unit is kept instead of being mapped to BoxedUnit.
   */
  def eraseResult(tp: Type)(implicit ctx: Context): Type =
    scalaErasureFn.eraseResult(tp)(erasureCtx)

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

    if (defn.isPolymorphicAfterErasure(sym)) eraseParamBounds(sym.info.asInstanceOf[PolyType])
    else if (sym.isAbstractType) TypeAlias(WildcardType)
    else if (sym.isConstructor) outer.addParam(sym.owner.asClass, erase(tp)(erasureCtx))
    else eraseInfo(tp, sym)(erasureCtx) match {
      case einfo: MethodType if sym.isGetter && einfo.resultType.isRef(defn.UnitClass) =>
        defn.BoxedUnitClass.typeRef
      case einfo =>
        einfo
    }
  }

  /** Is `tp` an abstract type or polymorphic type parameter that has `Any`
   *  as upper bound and that is not Java defined? Arrays of such types are
   *  erased to `Object` instead of `ObjectArray`.
   */
  def isUnboundedGeneric(tp: Type)(implicit ctx: Context): Boolean = tp.dealias match {
    case tp: TypeRef =>
      !tp.symbol.isClass &&
      !tp.derivesFrom(defn.ObjectClass) &&
      !tp.symbol.is(JavaDefined)
    case tp: PolyParam =>
      !tp.derivesFrom(defn.ObjectClass) &&
      !tp.binder.resultType.isInstanceOf[JavaMethodType]
    case tp: TypeProxy => isUnboundedGeneric(tp.underlying)
    case tp: AndType => isUnboundedGeneric(tp.tp1) || isUnboundedGeneric(tp.tp2)
    case tp: OrType => isUnboundedGeneric(tp.tp1) && isUnboundedGeneric(tp.tp2)
    case _ => false
  }


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
                if (!bc.is(Trait) && bc != defn.AnyClass) bc
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
   *   - For a typeref scala.Any, scala.AnyVal or scala.Singleton: |java.lang.Object|
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
  private def apply(tp: Type)(implicit ctx: Context): Type = tp match {
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
      this(tp.widen)
    case tp: ThisType =>
      this(tp.cls.typeRef)
    case SuperType(thistpe, supertpe) =>
      SuperType(this(thistpe), this(supertpe))
    case ExprType(rt) =>
      defn.FunctionClass(0).typeRef
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
          else classParents.mapConserve(eraseTypeRef) match {
            case tr :: trs1 =>
              assert(!tr.classSymbol.is(Trait), cls)
              val tr1 = if (cls is Trait) defn.ObjectClass.typeRef else tr
              tr1 :: trs1.filterNot(_ isRef defn.ObjectClass)
            case nil => nil
          }
        val erasedDecls = decls.filteredScope(d => !d.isType || d.isClass)
        tp.derivedClassInfo(NoPrefix, parents, erasedDecls, erasedRef(tp.selfType))
          // can't replace selftype by NoType because this would lose the sourceModule link
      }
    case NoType | NoPrefix | ErrorType | JavaArrayType(_) =>
      tp
    case tp: WildcardType if wildcardOK =>
      tp
  }

  private def eraseArray(tp: RefinedType)(implicit ctx: Context) = {
    val defn.ArrayType(elemtp) = tp
    if (elemtp derivesFrom defn.NullClass) JavaArrayType(defn.ObjectType)
    else if (isUnboundedGeneric(elemtp)) defn.ObjectType
    else JavaArrayType(this(elemtp))
  }

  def eraseInfo(tp: Type, sym: Symbol)(implicit ctx: Context) = tp match {
    case ExprType(rt) =>
      if (sym is Param) apply(tp)
        // Note that params with ExprTypes are eliminated by ElimByName,
        // but potentially re-introduced by ResolveSuper, when we add
        // forwarders to mixin methods.
        // See doc comment for ElimByName for speculation how we could improve this.
      else MethodType(Nil, Nil, eraseResult(rt))
    case tp => erasure(tp)
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
      if (cls == defn.AnyClass || cls == defn.AnyValClass || cls == defn.SingletonClass)
        return defn.ObjectClass
      if (cls == defn.UnitClass)
        return defn.BoxedUnitClass
    }
    cls
  }

  /** The name of the type as it is used in `Signature`s.
   *  Need to ensure correspondence with erasure!
   */
  private def sigName(tp: Type)(implicit ctx: Context): TypeName = tp match {
    case tp: TypeRef =>
      val sym = tp.symbol
      if (!sym.isClass) sigName(tp.info)
      else if (sym.isDerivedValueClass) sigName(eraseDerivedValueClassRef(tp))
      else normalizeClass(sym.asClass).fullName.asTypeName
    case defn.ArrayType(elem) =>
      sigName(this(tp))
    case JavaArrayType(elem) =>
      sigName(elem) ++ "[]"
    case tp: TermRef =>
      sigName(tp.widen)
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
