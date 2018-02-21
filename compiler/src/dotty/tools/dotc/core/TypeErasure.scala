package dotty.tools
package dotc
package core

import Symbols._, Types._, Contexts._, Flags._, Names._, StdNames._, Decorators._
import Flags.JavaDefined
import NameOps._
import Uniques.unique
import dotc.transform.ExplicitOuter._
import dotc.transform.ValueClasses._
import util.DotClass
import Definitions.MaxImplementedFunctionArity
import scala.annotation.tailrec

/** Erased types are:
 *
 *  ErasedValueType
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
 *  only for isInstanceOf, asInstanceOf: PolyType, TypeParamRef, TypeBounds
 *
 */
object TypeErasure {

  /** A predicate that tests whether a type is a legal erased type. Only asInstanceOf and
   *  isInstanceOf may have types that do not satisfy the predicate.
   *  ErasedValueType is considered an erased type because it is valid after Erasure (it is
   *  eliminated by ElimErasedValueType).
   */
  def isErasedType(tp: Type)(implicit ctx: Context): Boolean = tp match {
    case _: ErasedValueType =>
      true
    case tp: TypeRef =>
      val sym = tp.symbol
      sym.isClass &&
      sym != defn.AnyClass && sym != defn.ArrayClass &&
      !defn.isSyntheticFunctionClass(sym)
    case _: TermRef =>
      true
    case JavaArrayType(elem) =>
      isErasedType(elem)
    case AnnotatedType(tp, _) =>
      isErasedType(tp)
    case ThisType(tref) =>
      isErasedType(tref)
    case tp: MethodType =>
      tp.paramInfos.forall(isErasedType) && isErasedType(tp.resultType)
    case tp @ ClassInfo(pre, _, parents, decls, _) =>
      isErasedType(pre) && parents.forall(isErasedType) //&& decls.forall(sym => isErasedType(sym.info)) && isErasedType(tp.selfType)
    case NoType | NoPrefix | WildcardType | _: ErrorType | SuperType(_, _) =>
      true
    case _ =>
      false
  }

  /** A type representing the semi-erasure of a derived value class, see SIP-15
   *  where it's called "C$unboxed" for a class C.
   *  Derived value classes are erased to this type during Erasure (when
   *  semiEraseVCs = true) and subsequently erased to their underlying type
   *  during ElimErasedValueType. This type is outside the normal Scala class
   *  hierarchy: it is a subtype of no other type and is a supertype only of
   *  Nothing. This is because this type is only useful for type adaptation (see
   *  [[Erasure.Boxing#adaptToType]]).
   *
   *  @param   tycon             A TypeRef referring to the value class symbol
   *  @param   erasedUnderlying  The erased type of the single field of the value class
   */
  abstract case class ErasedValueType(tycon: TypeRef, erasedUnderlying: Type)
  extends CachedGroundType with ValueType {
    override def computeHash(bs: Hashable.Binders) = doHash(bs, tycon, erasedUnderlying)
  }

  final class CachedErasedValueType(tycon: TypeRef, erasedUnderlying: Type)
    extends ErasedValueType(tycon, erasedUnderlying)

  object ErasedValueType {
    def apply(tycon: TypeRef, erasedUnderlying: Type)(implicit ctx: Context) = {
      assert(erasedUnderlying.exists)
      unique(new CachedErasedValueType(tycon, erasedUnderlying))
    }
  }

  private def erasureIdx(isJava: Boolean, semiEraseVCs: Boolean, isConstructor: Boolean, wildcardOK: Boolean) =
    (if (isJava) 1 else 0) +
    (if (semiEraseVCs) 2 else 0) +
    (if (isConstructor) 4 else 0) +
    (if (wildcardOK) 8 else 0)

  private val erasures = new Array[TypeErasure](16)

  for {
    isJava <- List(false, true)
    semiEraseVCs <- List(false, true)
    isConstructor <- List(false, true)
    wildcardOK <- List(false, true)
  } erasures(erasureIdx(isJava, semiEraseVCs, isConstructor, wildcardOK)) =
    new TypeErasure(isJava, semiEraseVCs, isConstructor, wildcardOK)

  /** Produces an erasure function. See the documentation of the class [[TypeErasure]]
   *  for a description of each parameter.
   */
  private def erasureFn(isJava: Boolean, semiEraseVCs: Boolean, isConstructor: Boolean, wildcardOK: Boolean): TypeErasure =
    erasures(erasureIdx(isJava, semiEraseVCs, isConstructor, wildcardOK))

  /** The current context with a phase no later than erasure */
  private def erasureCtx(implicit ctx: Context) =
    if (ctx.erasedTypes) ctx.withPhase(ctx.erasurePhase) else ctx

  /** The standard erasure of a Scala type. Value classes are erased as normal classes.
   *
   *  @param tp            The type to erase.
  */
  def erasure(tp: Type)(implicit ctx: Context): Type =
    erasureFn(isJava = false, semiEraseVCs = false, isConstructor = false, wildcardOK = false)(tp)(erasureCtx)

  /** The value class erasure of a Scala type, where value classes are semi-erased to
   *  ErasedValueType (they will be fully erased in [[ElimErasedValueType]]).
   *
   *  @param tp            The type to erase.
   */
  def valueErasure(tp: Type)(implicit ctx: Context): Type =
    erasureFn(isJava = false, semiEraseVCs = true, isConstructor = false, wildcardOK = false)(tp)(erasureCtx)

  def sigName(tp: Type, isJava: Boolean)(implicit ctx: Context): TypeName = {
    val normTp =
      if (tp.isRepeatedParam) {
        val seqClass = if (isJava) defn.ArrayClass else defn.SeqClass
        tp.translateParameterized(defn.RepeatedParamClass, seqClass)
      }
      else tp
    val erase = erasureFn(isJava, semiEraseVCs = false, isConstructor = false, wildcardOK = true)
    erase.sigName(normTp)(erasureCtx)
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
      valueErasure(tp)
  }

  /**  The symbol's erased info. This is the type's erasure, except for the following symbols:
   *
   *   - For $asInstanceOf           : [T]T
   *   - For $isInstanceOf           : [T]Boolean
   *   - For all abstract types      : = ?
   *   - For companion methods       : the erasure of their type with semiEraseVCs = false.
   *                                   The signature of these methods are used to keep a
   *                                   link between companions and should not be semi-erased.
   *   - For Java-defined symbols:   : the erasure of their type with isJava = true,
   *                                   semiEraseVCs = false. Semi-erasure never happens in Java.
   *   - For all other symbols       : the semi-erasure of their types, with
   *                                   isJava, isConstructor set according to symbol.
   */
  def transformInfo(sym: Symbol, tp: Type)(implicit ctx: Context): Type = {
    val isJava = sym is JavaDefined
    val semiEraseVCs = !isJava && !sym.isCompanionMethod
    val erase = erasureFn(isJava, semiEraseVCs, sym.isConstructor, wildcardOK = false)

    def eraseParamBounds(tp: PolyType): Type =
      tp.derivedLambdaType(
        tp.paramNames, tp.paramNames map (Function.const(TypeBounds.upper(defn.ObjectType))), tp.resultType)

    if (defn.isPolymorphicAfterErasure(sym)) eraseParamBounds(sym.info.asInstanceOf[PolyType])
    else if (sym.isAbstractType) TypeAlias(WildcardType)
    else if (sym.isConstructor) outer.addParam(sym.owner.asClass, erase(tp)(erasureCtx))
    else erase.eraseInfo(tp, sym)(erasureCtx) match {
      case einfo: MethodType =>
        if (sym.isGetter && einfo.resultType.isRef(defn.UnitClass))
          MethodType(Nil, defn.BoxedUnitType)
        else if (sym.isAnonymousFunction && einfo.paramInfos.length > MaxImplementedFunctionArity)
          MethodType(nme.ALLARGS :: Nil, JavaArrayType(defn.ObjectType) :: Nil, einfo.resultType)
        else
          einfo
      case einfo =>
        einfo
    }
  }

  /** Is `tp` an abstract type or polymorphic type parameter that has `Any`, `AnyVal`,
   *  or a universal trait as upper bound and that is not Java defined? Arrays of such types are
   *  erased to `Object` instead of `Object[]`.
   */
  def isUnboundedGeneric(tp: Type)(implicit ctx: Context): Boolean = tp.dealias match {
    case tp: TypeRef =>
      !tp.symbol.isClass &&
      !tp.derivesFrom(defn.ObjectClass) &&
      !tp.symbol.is(JavaDefined)
    case tp: TypeParamRef =>
      !tp.derivesFrom(defn.ObjectClass) &&
      !tp.binder.resultType.isJavaMethod
    case tp: TypeAlias => isUnboundedGeneric(tp.alias)
    case tp: TypeBounds => !tp.hi.derivesFrom(defn.ObjectClass)
    case tp: TypeProxy => isUnboundedGeneric(tp.underlying)
    case tp: AndType => isUnboundedGeneric(tp.tp1) && isUnboundedGeneric(tp.tp2)
    case tp: OrType => isUnboundedGeneric(tp.tp1) || isUnboundedGeneric(tp.tp2)
    case _ => false
  }

  /** The erased least upper bound is computed as follows
   *  - if both argument are arrays of objects, an array of the erased lub of the element types
   *  - if both arguments are arrays of same primitives, an array of this primitive
   *  - if one argument is array of primitives and the other is array of objects, Object
   *  - if one argument is an array, Object
   *  - otherwise a common superclass or trait S of the argument classes, with the
   *    following two properties:
   *      S is minimal: no other common superclass or trait derives from S
   *      S is last   : in the linearization of the first argument type `tp1`
   *                    there are no minimal common superclasses or traits that
   *                    come after S.
   *  The reason to pick last is that we prefer classes over traits that way,
   *  which leads to more predictable bytecode and (?) faster dynamic dispatch.
   */
  def erasedLub(tp1: Type, tp2: Type)(implicit ctx: Context): Type = tp1 match {
    case JavaArrayType(elem1) =>
      import dotty.tools.dotc.transform.TypeUtils._
      tp2 match {
        case JavaArrayType(elem2) =>
          if (elem1.isPrimitiveValueType || elem2.isPrimitiveValueType) {
            if (elem1.classSymbol eq elem2.classSymbol) // same primitive
              JavaArrayType(elem1)
            else defn.ObjectType
          } else JavaArrayType(erasedLub(elem1, elem2))
        case _ => defn.ObjectType
      }
    case _ =>
      tp2 match {
        case JavaArrayType(_) => defn.ObjectType
        case _ =>
          val cls2 = tp2.classSymbol

          /** takeWhile+1 */
          def takeUntil[T](l: List[T])(f: T => Boolean): List[T] = {
            @tailrec def loop(tail: List[T], acc: List[T]): List[T] =
              tail match {
                case h :: t => loop(if (f(h)) t else Nil, h :: acc)
                case Nil    => acc.reverse
              }
            loop(l, Nil)
          }

          // We are not interested in anything that is not a supertype of tp2
          val tp2superclasses = tp1.baseClasses.filter(cls2.derivesFrom)

          // From the spec, "Linearization also satisfies the property that a
          // linearization of a class always contains the linearization of its
          // direct superclass as a suffix"; it's enought to consider every
          // candidate up to the first class.
          val candidates = takeUntil(tp2superclasses)(!_.is(Trait))

          // Candidates st "no other common superclass or trait derives from S"
          val minimums = candidates.filter { cand =>
            candidates.forall(x => !x.derivesFrom(cand) || x.eq(cand))
          }

          // Pick the last minimum to prioritise classes over traits
          minimums.lastOption match {
            case Some(lub) if lub != defn.AnyClass && lub != defn.AnyValClass =>
              lub.typeRef
            case _ => // Any/AnyVal only exist before erasure
              defn.ObjectType
          }
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

  /** Does the (possibly generic) type `tp` have the same erasure in all its
   *  possible instantiations?
   */
  def hasStableErasure(tp: Type)(implicit ctx: Context): Boolean = tp match {
    case tp: TypeRef =>
      tp.info match {
        case TypeAlias(alias) => hasStableErasure(alias)
        case _: ClassInfo => true
        case _ => false
      }
    case tp: TypeParamRef => false
    case tp: TypeBounds => false
    case tp: TypeProxy => hasStableErasure(tp.superType)
    case tp: AndOrType => hasStableErasure(tp.tp1) && hasStableErasure(tp.tp2)
    case _ => false
  }
}
import TypeErasure._

/**
 *  @param isJava        Arguments should be treated the way Java does it
 *  @param semiEraseVCs  If true, value classes are semi-erased to ErasedValueType
 *                       (they will be fully erased in [[ElimErasedValueType]]).
 *                       If false, they are erased like normal classes.
 *  @param isConstructor Argument forms part of the type of a constructor
 *  @param wildcardOK    Wildcards are acceptable (true when using the erasure
 *                       for computing a signature name).
 */
class TypeErasure(isJava: Boolean, semiEraseVCs: Boolean, isConstructor: Boolean, wildcardOK: Boolean) extends DotClass {

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
   *   - For a typeref scala.FunctionN, where N > MaxImplementedFunctionArity, scala.FunctionXXL
   *   - For a typeref scala.ImplicitFunctionN, | scala.FunctionN |
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
    case _: ErasedValueType =>
      tp
    case tp: TypeRef =>
      val sym = tp.symbol
      if (!sym.isClass) this(tp.info)
      else if (semiEraseVCs && isDerivedValueClass(sym)) eraseDerivedValueClassRef(tp)
      else if (sym == defn.ArrayClass) apply(tp.appliedTo(TypeBounds.empty)) // i966 shows that we can hit a raw Array type.
      else if (defn.isSyntheticFunctionClass(sym)) defn.erasedFunctionType(sym)
      else if (defn.isPhantomTerminalClass(sym)) PhantomErasure.erasedPhantomType
      else if (sym eq defn.PhantomClass) defn.ObjectType // To erase the definitions of Phantom.{assume, Any, Nothing}
      else eraseNormalClassRef(tp)
    case tp: AppliedType =>
      if (tp.tycon.isRef(defn.ArrayClass)) eraseArray(tp)
      else apply(tp.superType)
    case _: TermRef | _: ThisType =>
      this(tp.widen)
    case SuperType(thistpe, supertpe) =>
      SuperType(this(thistpe), this(supertpe))
    case ExprType(rt) =>
      defn.FunctionType(0)
    case tp: TypeProxy =>
      this(tp.underlying)
    case AndType(tp1, tp2) =>
      erasedGlb(this(tp1), this(tp2), isJava)
    case OrType(tp1, tp2) =>
      ctx.typeComparer.orType(this(tp1), this(tp2), erased = true)
    case tp: MethodType =>
      def paramErasure(tpToErase: Type) =
        erasureFn(tp.isJavaMethod, semiEraseVCs, isConstructor, wildcardOK)(tpToErase)
      val (names, formals0) =
        if (tp.isUnusedMethod) (Nil, Nil)
        else if (tp.paramInfos.exists(_.isPhantom)) tp.paramNames.zip(tp.paramInfos).filterNot(_._2.isPhantom).unzip
        else (tp.paramNames, tp.paramInfos)
      val formals = formals0.mapConserve(paramErasure)
      eraseResult(tp.resultType) match {
        case rt: MethodType =>
          tp.derivedLambdaType(names ++ rt.paramNames, formals ++ rt.paramInfos, rt.resultType)
        case rt =>
          tp.derivedLambdaType(names, formals, rt)
      }
    case tp: PolyType =>
      this(tp.resultType)
    case tp @ ClassInfo(pre, cls, parents, decls, _) =>
      if (cls is Package) tp
      else {
        val erasedParents: List[Type] =
          if ((cls eq defn.ObjectClass) || cls.isPrimitiveValueClass) Nil
          else parents.mapConserve(apply) match {
            case tr :: trs1 =>
              assert(!tr.classSymbol.is(Trait), cls)
              val tr1 = if (cls is Trait) defn.ObjectType else tr
              tr1 :: trs1.filterNot(_ isRef defn.ObjectClass)
            case nil => nil
          }
        val erasedDecls = decls.filteredScope(sym => !sym.isType || sym.isClass)
        tp.derivedClassInfo(NoPrefix, erasedParents, erasedDecls, erasedRef(tp.selfType))
          // can't replace selftype by NoType because this would lose the sourceModule link
      }
    case _: ErrorType | JavaArrayType(_) =>
      tp
    case tp: WildcardType if wildcardOK =>
      tp
    case tp if (tp `eq` NoType) || (tp `eq` NoPrefix) =>
      tp
  }

  private def eraseArray(tp: Type)(implicit ctx: Context) = {
    val defn.ArrayOf(elemtp) = tp
    def arrayErasure(tpToErase: Type) =
      erasureFn(isJava, semiEraseVCs = false, isConstructor, wildcardOK)(tpToErase)
    if (elemtp derivesFrom defn.NullClass) JavaArrayType(defn.ObjectType)
    else if (isUnboundedGeneric(elemtp) && !isJava) defn.ObjectType
    else JavaArrayType(arrayErasure(elemtp))
  }

  /** The erasure of a symbol's info. This is different from `apply` in the way `ExprType`s and
   *  `PolyType`s are treated. `eraseInfo` maps them them to method types, whereas `apply` maps them
   *  to the underlying type.
   */
  def eraseInfo(tp: Type, sym: Symbol)(implicit ctx: Context) = tp match {
    case ExprType(rt) =>
      if (sym is Param) apply(tp)
        // Note that params with ExprTypes are eliminated by ElimByName,
        // but potentially re-introduced by ResolveSuper, when we add
        // forwarders to mixin methods.
        // See doc comment for ElimByName for speculation how we could improve this.
      else MethodType(Nil, Nil, eraseResult(sym.info.finalResultType))
    case tp: PolyType =>
      eraseResult(tp.resultType) match {
        case rt: MethodType => rt
        case rt => MethodType(Nil, Nil, rt)
      }
    case tp => this(tp)
  }

  private def eraseDerivedValueClassRef(tref: TypeRef)(implicit ctx: Context): Type = {
    val cls = tref.symbol.asClass
    val underlying = underlyingOfValueClass(cls)
    if (underlying.exists && !isCyclic(cls)) ErasedValueType(tref, valueErasure(underlying))
    else NoType
  }

  private def eraseNormalClassRef(tref: TypeRef)(implicit ctx: Context): Type = {
    val cls = tref.symbol.asClass
    (if (cls.owner is Package) normalizeClass(cls) else cls).typeRef
  }

  /** The erasure of a function result type. */
  private def eraseResult(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: TypeRef =>
      val sym = tp.typeSymbol
      if (sym eq defn.UnitClass) sym.typeRef
      // For a value class V, "new V(x)" should have type V for type adaptation to work
      // correctly (see SIP-15 and [[Erasure.Boxing.adaptToType]]), so the return type of a
      // constructor method should not be semi-erased.
      else if (isConstructor && isDerivedValueClass(sym)) eraseNormalClassRef(tp)
      else this(tp)
    case AppliedType(tycon, _) if !(tycon isRef defn.ArrayClass) =>
      eraseResult(tycon)
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
  private def sigName(tp: Type)(implicit ctx: Context): TypeName = try {
    tp match {
      case tp: TypeRef =>
        if (!tp.denot.exists) {
          // println(i"missing: ${tp.toString} ${tp.denot} / ${tp.prefix.member(tp.name)}")
          throw new MissingType(tp.prefix, tp.name)
        }
        val sym = tp.symbol
        if (!sym.isClass) {
          val info = tp.info
          if (!info.exists) assert(false, "undefined: $tp with symbol $sym")
          return sigName(info)
        }
        if (isDerivedValueClass(sym)) {
          val erasedVCRef = eraseDerivedValueClassRef(tp)
          if (erasedVCRef.exists) return sigName(erasedVCRef)
        }
        if (defn.isSyntheticFunctionClass(sym))
          sigName(defn.erasedFunctionType(sym))
        else if (defn.isPhantomTerminalClass(tp.symbol))
          sigName(PhantomErasure.erasedPhantomType)
        else
          normalizeClass(sym.asClass).fullName.asTypeName
      case tp: AppliedType =>
        sigName(
          if (tp.tycon.isRef(defn.ArrayClass)) this(tp)
          else if (tp.tycon.typeSymbol.isClass) tp.underlying
          else tp.superType)
      case ErasedValueType(_, underlying) =>
        sigName(underlying)
      case JavaArrayType(elem) =>
        sigName(elem) ++ "[]"
      case tp: TermRef =>
        sigName(tp.widen)
      case ExprType(rt) =>
        sigName(defn.FunctionOf(Nil, rt))
      case tp: TypeVar =>
        val inst = tp.instanceOpt
        if (inst.exists) sigName(inst) else tpnme.Uninstantiated
      case tp: TypeProxy =>
        sigName(tp.underlying)
      case _: ErrorType | WildcardType =>
        tpnme.WILDCARD
      case tp: WildcardType =>
        sigName(tp.optBounds)
      case _ =>
        val erased = this(tp)
        assert(erased ne tp, tp)
        sigName(erased)
    }
  } catch {
    case ex: AssertionError =>
      println(s"no sig for $tp because of ${ex.printStackTrace()}")
      throw ex
  }


}
