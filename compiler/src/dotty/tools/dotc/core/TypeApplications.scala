package dotty.tools
package dotc
package core

import Types._
import Contexts._
import Symbols._
import SymDenotations.LazyType
import Decorators._
import util.Stats._
import Names._
import NameOps._
import dotty.tools.dotc.config.Config

object TypeApplications {

  type TypeParamInfo = ParamInfo.Of[TypeName]

  /** Assert type is not a TypeBounds instance and return it unchanged */
  def noBounds(tp: Type): Type = tp match {
    case tp: TypeBounds => throw new AssertionError("no TypeBounds allowed")
    case _ => tp
  }

  /** Does variance `v1` conform to variance `v2`?
   *  This is the case if the variances are the same or `sym` is nonvariant.
   */
  def varianceConforms(v1: Int, v2: Int): Boolean =
    v1 == v2 || v2 == 0

  /** Does the variance of type parameter `tparam1` conform to the variance of type parameter `tparam2`?
   */
  def varianceConforms(tparam1: TypeParamInfo, tparam2: TypeParamInfo)(implicit ctx: Context): Boolean =
    varianceConforms(tparam1.paramVariance, tparam2.paramVariance)

  /** Do the variances of type parameters `tparams1` conform to the variances
   *  of corresponding type parameters `tparams2`?
   *  This is only the case of `tparams1` and `tparams2` have the same length.
   */
  def variancesConform(tparams1: List[TypeParamInfo], tparams2: List[TypeParamInfo])(implicit ctx: Context): Boolean =
    tparams1.corresponds(tparams2)(varianceConforms)

  /** Extractor for
   *
   *    [v1 X1: B1, ..., vn Xn: Bn] -> C[X1, ..., Xn]
   *
   *  where v1, ..., vn and B1, ..., Bn are the variances and bounds of the type parameters
   *  of the class C.
   *
   *  @param tycon     C
   */
  object EtaExpansion {
    def apply(tycon: Type)(implicit ctx: Context): Type = {
      assert(tycon.typeParams.nonEmpty, tycon)
      tycon.EtaExpand(tycon.typeParamSymbols)
    }

    def unapply(tp: Type)(implicit ctx: Context): Option[TypeRef] = tp match {
      case tp @ HKTypeLambda(tparams, AppliedType(fn: TypeRef, args)) if (args == tparams.map(_.paramRef)) => Some(fn)
      case _ => None
    }
  }

   /** Adapt all arguments to possible higher-kinded type parameters using etaExpandIfHK
   */
  def EtaExpandIfHK(tparams: List[TypeParamInfo], args: List[Type])(implicit ctx: Context): List[Type] =
    if (tparams.isEmpty) args
    else args.zipWithConserve(tparams)((arg, tparam) => arg.EtaExpandIfHK(tparam.paramInfoOrCompleter))

  /** A type map that tries to reduce (part of) the result type of the type lambda `tycon`
   *  with the given `args`(some of which are wildcard arguments represented by type bounds).
   *  Non-wildcard arguments are substituted everywhere as usual. A wildcard argument
   *  `>: L <: H` is substituted for a type lambda parameter `X` only under certain conditions.
   *
   *  1. If Mode.AllowLambdaWildcardApply is set:
   *  The wildcard argument is substituted only if `X` appears in a toplevel application of the form
   *
   *        C[..., X, ...]
   *
   *  and there are no other occurrences of `X` in the reduced type. In that case
   *  the refinement above is replaced by
   *
   *        C[..., _ >: L <: H, ...]
   *
   *  The `allReplaced` field indicates whether all occurrences of type lambda parameters
   *  in the reduced type have been replaced with arguments.
   *
   *  2. If Mode.AllowLambdaWildcardApply is not set:
   *  All `X` arguments are replaced by:
   *
   *        _ >: L <: H
   *
   *  Any other occurrence of `X` in `tycon` is replaced by `U`, if the
   *  occurrence of `X` in `tycon` is covariant, or nonvariant, or by `L`,
   *  if the occurrence is contravariant.
   *
   *  The idea is that the `AllowLambdaWildcardApply` mode is used to check whether
   *  a type can be soundly reduced, and to give an error or warning if that
   *  is not the case. By contrast, the default mode, with `AllowLambdaWildcardApply`
   *  not set, reduces all applications even if this yields a different type, so
   *  its postcondition is that no type parameters of `tycon` appear in the
   *  result type. Using this mode, we can guarantee that `appliedTo` will never
   *  produce a higher-kinded application with a type lambda as type constructor.
   */
  class Reducer(tycon: TypeLambda, args: List[Type])(implicit ctx: Context) extends TypeMap {
    private[this] var available = (0 until args.length).toSet
    var allReplaced: Boolean = true
    def hasWildcardArg(p: TypeParamRef): Boolean =
      p.binder == tycon && isBounds(args(p.paramNum))
    def canReduceWildcard(p: TypeParamRef): Boolean =
      !ctx.mode.is(Mode.AllowLambdaWildcardApply) || available.contains(p.paramNum)
    def atNestedLevel(op: => Type): Type = {
      val saved = available
      available = Set()
      try op
      finally available = saved
    }

    // If this is a reference to a reducable type parameter corresponding to a
    // wildcard argument, return the wildcard argument, otherwise apply recursively.
    def applyArg(arg: Type): Type = arg match {
      case p: TypeParamRef if hasWildcardArg(p) && canReduceWildcard(p) =>
        available -= p.paramNum
        args(p.paramNum)
      case _ =>
        atNestedLevel(apply(arg))
    }

    def apply(t: Type): Type = t match {
      case t @ AppliedType(tycon, args1) if tycon.typeSymbol.isClass =>
        t.derivedAppliedType(apply(tycon), args1.mapConserve(applyArg))
      case t @ RefinedType(parent, name, TypeAlias(info)) =>
        t.derivedRefinedType(apply(parent), name, applyArg(info).bounds)
      case p: TypeParamRef if p.binder == tycon =>
        args(p.paramNum) match {
          case TypeBounds(lo, hi) =>
            if (ctx.mode.is(Mode.AllowLambdaWildcardApply)) { allReplaced = false; p }
            else if (variance < 0) lo
            else hi
          case arg =>
            arg
        }
      case _: TypeBounds | _: AppliedType =>
        atNestedLevel(mapOver(t))
      case _ =>
        mapOver(t)
    }
  }
}

import TypeApplications._

/** A decorator that provides methods for modeling type application */
class TypeApplications(val self: Type) extends AnyVal {

  /** The type parameters of this type are:
   *  For a ClassInfo type, the type parameters of its class.
   *  For a typeref referring to a class, the type parameters of the class.
   *  For a typeref referring to a Lambda class, the type parameters of
   *    its right hand side or upper bound.
   *  For a refinement type, the type parameters of its parent, dropping
   *  any type parameter that is-rebound by the refinement.
   */
  final def typeParams(implicit ctx: Context): List[TypeParamInfo] = /*>|>*/ track("typeParams") /*<|<*/ {
    def isTrivial(prefix: Type, tycon: Symbol) = prefix match {
      case prefix: ThisType => prefix.cls `eq` tycon.owner
      case NoPrefix => true
      case _ => false
    }
    try self match {
      case self: TypeRef =>
        val tsym = self.symbol
        if (tsym.isClass) tsym.typeParams
        else tsym.infoOrCompleter match {
          case info: LazyType if isTrivial(self.prefix, tsym) => info.completerTypeParams(tsym)
          case _ => self.info.typeParams
        }
      case self: AppliedType =>
        if (self.tycon.typeSymbol.isClass) Nil
        else self.superType.typeParams
      case self: ClassInfo =>
        self.cls.typeParams
      case self: HKTypeLambda =>
        self.typeParams
      case _: SingletonType | _: RefinedType | _: RecType =>
        Nil
      case self: WildcardType =>
        self.optBounds.typeParams
      case self: TypeProxy =>
        self.superType.typeParams
      case _ =>
        Nil
    }
    catch {
      case ex: Throwable => handleRecursive("type parameters of", self.show, ex)
    }
  }

  /** If `self` is a higher-kinded type, its type parameters, otherwise Nil */
  final def hkTypeParams(implicit ctx: Context): List[TypeParamInfo] =
    if (isLambdaSub) typeParams else Nil

  /** If `self` is a generic class, its type parameter symbols, otherwise Nil */
  final def typeParamSymbols(implicit ctx: Context): List[TypeSymbol] = typeParams match {
    case (_: Symbol) :: _ =>
      assert(typeParams.forall(_.isInstanceOf[Symbol]))
      typeParams.asInstanceOf[List[TypeSymbol]]
    case _ => Nil
  }

  /** Is self type bounded by a type lambda or AnyKind? */
  def isLambdaSub(implicit ctx: Context): Boolean = hkResult.exists

  /** Is self type of kind "*"? */
  def hasSimpleKind(implicit ctx: Context): Boolean =
    typeParams.isEmpty && !self.hasAnyKind || {
      val alias = self.dealias
      (alias ne self) && alias.hasSimpleKind
    }

  /** If self type is higher-kinded, its result type, otherwise NoType.
   *  Note: The hkResult of an any-kinded type is again AnyKind.
   */
  def hkResult(implicit ctx: Context): Type = self.dealias match {
    case self: TypeRef =>
      if (self.symbol == defn.AnyKindClass) self else self.info.hkResult
    case self: AppliedType =>
      if (self.tycon.typeSymbol.isClass) NoType else self.superType.hkResult
    case self: HKTypeLambda => self.resultType
    case _: SingletonType | _: RefinedType | _: RecType => NoType
    case self: WildcardType => self.optBounds.hkResult
    case self: TypeVar =>
      // Using `origin` instead of `underlying`, as is done for typeParams,
      // avoids having to set ephemeral in some cases.
      self.origin.hkResult
    case self: TypeProxy => self.superType.hkResult
    case _ => NoType
  }

  /** Do self and other have the same kinds (not counting bounds and variances)?
   *  Note: An any-kinded type "has the same kind" as any other type.
   */
  def hasSameKindAs(other: Type)(implicit ctx: Context): Boolean = {
    def isAnyKind(tp: Type) = tp match {
      case tp: TypeRef => tp.symbol == defn.AnyKindClass
      case _ => false
    }
    val selfResult = self.hkResult
    val otherResult = other.hkResult
    isAnyKind(selfResult) || isAnyKind(otherResult) ||
    { if (selfResult.exists)
        otherResult.exists &&
        selfResult.hasSameKindAs(otherResult) &&
        self.typeParams.corresponds(other.typeParams)((sparam, oparam) =>
          sparam.paramInfo.hasSameKindAs(oparam.paramInfo))
      else !otherResult.exists
    }
  }

  /** Dealias type if it can be done without forcing the TypeRef's info */
  def safeDealias(implicit ctx: Context): Type = self match {
    case self: TypeRef if self.denot.exists && self.symbol.isAliasType =>
      self.superType.stripTypeVar.safeDealias
    case _ =>
      self
  }

  /** Convert a type constructor `TC` which has type parameters `X1, ..., Xn`
   *  to `[X1, ..., Xn] -> TC[X1, ..., Xn]`.
   */
  def EtaExpand(tparams: List[TypeSymbol])(implicit ctx: Context): Type = {
    val tparamsToUse = if (variancesConform(typeParams, tparams)) tparams else typeParamSymbols
    HKTypeLambda.fromParams(tparamsToUse, self.appliedTo(tparams.map(_.typeRef)))
      //.ensuring(res => res.EtaReduce =:= self, s"res = $res, core = ${res.EtaReduce}, self = $self, hc = ${res.hashCode}")
  }

  /** If self is not lambda-bound, eta expand it. */
  def ensureLambdaSub(implicit ctx: Context): Type =
    if (isLambdaSub) self else EtaExpansion(self)

  /** Eta expand if `self` is a (non-lambda) class reference and `bound` is a higher-kinded type */
  def EtaExpandIfHK(bound: Type)(implicit ctx: Context): Type = {
    val hkParams = bound.hkTypeParams
    if (hkParams.isEmpty) self
    else self match {
      case self: TypeRef if self.symbol.isClass && self.typeParams.length == hkParams.length =>
        EtaExpansion(self)
      case _ => self
    }
  }

  /** If argument A and type parameter P are higher-kinded, adapt the variances
   *  of A to those of P, ensuring that the variances of the type lambda A
   *  agree with the variances of corresponding higher-kinded type parameters of P. Example:
   *
   *     class GenericCompanion[+CC[X]]
   *     GenericCompanion[List]
   *
   *  with adaptHkVariances, the argument `List` will expand to
   *
   *     [X] => List[X]
   *
   *  instead of
   *
   *     [+X] => List[X]
   *
   *  even though `List` is covariant. This adaptation is necessary to ignore conflicting
   *  variances in overriding members that have types of hk-type parameters such as
   *  `GenericCompanion[GenTraversable]` or `GenericCompanion[ListBuffer]`.
   *  When checking overriding, we need to validate the subtype relationship
   *
   *      GenericCompanion[[X] -> ListBuffer[X]] <: GenericCompanion[[+X] -> GenTraversable[X]]
   *
   *   Without adaptation, this would be false, and hence an overriding error would
   *   result. But with adaptation, the rhs argument will be adapted to
   *
   *     [X] -> GenTraversable[X]
   *
   *   which makes the subtype test succeed. The crucial point here is that, since
   *   GenericCompanion only expects a non-variant CC, the fact that GenTraversable
   *   is covariant is irrelevant, so can be ignored.
   */
  def adaptHkVariances(bound: Type)(implicit ctx: Context): Type = {
    val hkParams = bound.hkTypeParams
    if (hkParams.isEmpty) self
    else {
      def adaptArg(arg: Type): Type = arg match {
        case arg @ HKTypeLambda(tparams, body) if
             !tparams.corresponds(hkParams)(_.paramVariance == _.paramVariance) &&
             tparams.corresponds(hkParams)(varianceConforms) =>
          HKTypeLambda(
            (tparams, hkParams).zipped.map((tparam, hkparam) =>
              tparam.paramName.withVariance(hkparam.paramVariance)))(
            tl => arg.paramInfos.map(_.subst(arg, tl).bounds),
            tl => arg.resultType.subst(arg, tl)
          )
        case arg: AliasingBounds =>
          arg.derivedAlias(adaptArg(arg.alias))
        case arg @ TypeBounds(lo, hi) =>
          arg.derivedTypeBounds(adaptArg(lo), adaptArg(hi))
        case _ =>
          arg
      }
      adaptArg(self)
    }
  }

  /** The type representing
   *
   *     T[U1, ..., Un]
   *
   *  where
   *  @param  self   = `T`
   *  @param  args   = `U1,...,Un`
   */
  final def appliedTo(args: List[Type])(implicit ctx: Context): Type = /*>|>*/ track("appliedTo") /*<|<*/ {
    val typParams = self.typeParams
    val stripped = self.stripTypeVar
    val dealiased = stripped.safeDealias
    if (args.isEmpty || ctx.erasedTypes) self
    else dealiased match {
      case dealiased: HKTypeLambda =>
        def tryReduce =
          if (!args.exists(isBounds)) {
            val followAlias = Config.simplifyApplications && {
              dealiased.resType match {
                case AppliedType(tyconBody, dealiasedArgs) =>
                  // Reduction should not affect type inference when it's
                  // just eta-reduction (ignoring variance annotations).
                  // See i2201*.scala for examples where more aggressive
                  // reduction would break type inference.
                  dealiased.paramRefs == dealiasedArgs
                case _ => false
              }
            }
            if ((dealiased eq stripped) || followAlias)
              try dealiased.instantiate(args)
              catch { case ex: IndexOutOfBoundsException => AppliedType(self, args) }
            else AppliedType(self, args)
          }
          else dealiased.resType match {
            case AppliedType(tycon, args1) if tycon.safeDealias ne tycon =>
              // In this case we should always dealias since we cannot handle
              // higher-kinded applications to wildcard arguments.
              dealiased
                .derivedLambdaType(resType = tycon.safeDealias.appliedTo(args1))
                .appliedTo(args)
            case _ =>
              val reducer = new Reducer(dealiased, args)
              val reduced = reducer(dealiased.resType)
              if (reducer.allReplaced) reduced
              else AppliedType(dealiased, args)
          }
        tryReduce
      case dealiased: PolyType =>
        dealiased.instantiate(args)
      case dealiased: AndType =>
        dealiased.derivedAndType(dealiased.tp1.appliedTo(args), dealiased.tp2.appliedTo(args))
      case dealiased: OrType =>
        dealiased.derivedOrType(dealiased.tp1.appliedTo(args), dealiased.tp2.appliedTo(args))
      case dealiased: AliasingBounds =>
        dealiased.derivedAlias(dealiased.alias.appliedTo(args))
      case dealiased: TypeBounds =>
        dealiased.derivedTypeBounds(dealiased.lo.appliedTo(args), dealiased.hi.appliedTo(args))
      case dealiased: LazyRef =>
        LazyRef(c => dealiased.ref(c).appliedTo(args))
      case dealiased: WildcardType =>
        WildcardType(dealiased.optBounds.orElse(TypeBounds.empty).appliedTo(args).bounds)
      case dealiased: TypeRef if dealiased.symbol == defn.NothingClass =>
        dealiased
      case dealiased =>
        AppliedType(self, args)
    }
  }

  final def appliedTo(arg: Type)(implicit ctx: Context): Type = appliedTo(arg :: Nil)
  final def appliedTo(arg1: Type, arg2: Type)(implicit ctx: Context): Type = appliedTo(arg1 :: arg2 :: Nil)

  final def applyIfParameterized(args: List[Type])(implicit ctx: Context): Type =
    if (typeParams.nonEmpty) appliedTo(args) else self

  /** A cycle-safe version of `appliedTo` where computing type parameters do not force
   *  the typeconstructor. Instead, if the type constructor is completing, we make
   *  up hk type parameters matching the arguments. This is needed when unpickling
   *  Scala2 files such as `scala.collection.generic.Mapfactory`.
   */
  final def safeAppliedTo(args: List[Type])(implicit ctx: Context): Type = self match {
    case self: TypeRef if !self.symbol.isClass && self.symbol.isCompleting =>
      AppliedType(self, args)
    case _ =>
      appliedTo(args)
  }

  /** Turns non-bounds types to type bounds.
   *  A (possible lambda abstracted) match type is turned into an abstract type.
   *  Every other type is turned into a type alias
   */
  final def toBounds(implicit ctx: Context): TypeBounds = self match {
    case self: TypeBounds => self // this can happen for wildcard args
    case _ => if (self.isMatch) MatchAlias(self) else TypeAlias(self)
  }

  /** Translate a type of the form From[T] to either To[T] or To[_ <: T] (if `wildcardArg` is set). Keep other types as they are.
   *  `from` and `to` must be static classes, both with one type parameter, and the same variance.
   *  Do the same for by name types => From[T] and => To[T]
   */
  def translateParameterized(from: ClassSymbol, to: ClassSymbol, wildcardArg: Boolean = false)(implicit ctx: Context): Type = self match {
    case self @ ExprType(tp) =>
      self.derivedExprType(tp.translateParameterized(from, to))
    case _ =>
      if (self.derivesFrom(from)) {
        val arg = self.baseType(from).argInfos.head
        val arg1 = if (wildcardArg) TypeBounds.upper(arg) else arg
        to.typeRef.appliedTo(arg1)
      }
      else self
  }

  /** If this is repeated parameter type, its underlying Seq type,
   *  or, if isJava is true, Array type, else the type itself.
   */
  def underlyingIfRepeated(isJava: Boolean)(implicit ctx: Context): Type =
    if (self.isRepeatedParam) {
      val seqClass = if (isJava) defn.ArrayClass else defn.SeqClass
      // If `isJava` is set, then we want to turn `RepeatedParam[T]` into `Array[_ <: T]`,
      // since arrays aren't covariant until after erasure. See `tests/pos/i5140`.
      translateParameterized(defn.RepeatedParamClass, seqClass, wildcardArg = isJava)
    }
    else self

  /** If this is an encoding of a (partially) applied type, return its arguments,
   *  otherwise return Nil.
   *  Existential types in arguments are returned as TypeBounds instances.
   */
  final def argInfos(implicit ctx: Context): List[Type] = self.stripTypeVar.stripAnnots match {
    case AppliedType(tycon, args) => args
    case _ => Nil
  }

  /** Argument types where existential types in arguments are disallowed */
  def argTypes(implicit ctx: Context): List[Type] = argInfos mapConserve noBounds

  /** Argument types where existential types in arguments are approximated by their lower bound */
  def argTypesLo(implicit ctx: Context): List[Type] = argInfos.mapConserve(_.loBound)

  /** Argument types where existential types in arguments are approximated by their upper bound  */
  def argTypesHi(implicit ctx: Context): List[Type] = argInfos.mapConserve(_.hiBound)

  /** If this is the image of a type argument; recover the type argument,
   *  otherwise NoType.
   */
  final def argInfo(implicit ctx: Context): Type = self match {
    case self: TypeAlias => self.alias
    case self: TypeBounds => self
    case _ => NoType
  }

  /** If this is a type alias, its underlying type, otherwise the type itself */
  def dropAlias(implicit ctx: Context): Type = self match {
    case TypeAlias(alias) => alias
    case _ => self
  }

  /** The element type of a sequence or array */
  def elemType(implicit ctx: Context): Type = self.widenDealias match {
    case defn.ArrayOf(elemtp) => elemtp
    case JavaArrayType(elemtp) => elemtp
    case _ => self.baseType(defn.SeqClass).argInfos.headOption.getOrElse(NoType)
  }
}
