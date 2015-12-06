package dotty.tools.dotc
package core

import Types._
import Contexts._
import Symbols._
import Decorators._
import util.Stats._
import util.common._
import Names._
import NameOps._
import Flags._
import StdNames.tpnme
import typer.Mode
import util.Positions.Position
import config.Printers._
import collection.mutable
import java.util.NoSuchElementException

object TypeApplications {

  /** Assert type is not a TypeBounds instance and return it unchanged */
  val noBounds = (tp: Type) => tp match {
    case tp: TypeBounds => throw new AssertionError("no TypeBounds allowed")
    case _ => tp
  }

  /** If `tp` is a TypeBounds instance return its lower bound else return `tp` */
  val boundsToLo = (tp: Type) => tp match {
    case tp: TypeBounds => tp.lo
    case _ => tp
  }

  /** If `tp` is a TypeBounds instance return its upper bound else return `tp` */
  val boundsToHi = (tp: Type) => tp match {
    case tp: TypeBounds => tp.hi
    case _ => tp
  }

  /** Does the variance of `sym1` conform to the variance of `sym2`?
   *  This is the case if the variances are the same or `sym` is nonvariant.
   */
  def varianceConforms(sym1: TypeSymbol, sym2: TypeSymbol)(implicit ctx: Context) =
    sym1.variance == sym2.variance || sym2.variance == 0

  def variancesConform(syms1: List[TypeSymbol], syms2: List[TypeSymbol])(implicit ctx: Context) =
    syms1.corresponds(syms2)(varianceConforms)

  /** Extractor for
   *
   *    [v1 X1: B1, ..., vn Xn: Bn] -> T
   *    ==>
   *    Lambda$_v1...vn { type $hk_i: B_i, type $Apply = [X_i := this.$Arg_i] T }
   */
  object TypeLambda {
    def apply(variances: List[Int],
              argBoundsFns: List[RefinedType => TypeBounds],
              bodyFn: RefinedType => Type)(implicit ctx: Context): Type = {
      def argRefinements(parent: Type, i: Int, bs: List[RefinedType => TypeBounds]): Type = bs match {
        case b :: bs1 =>
          argRefinements(RefinedType(parent, tpnme.hkArg(i), b), i + 1, bs1)
        case nil =>
          parent
      }
      assert(variances.nonEmpty)
      assert(argBoundsFns.length == variances.length)
      RefinedType(
        argRefinements(defn.LambdaTrait(variances).typeRef, 0, argBoundsFns),
        tpnme.hkApply, bodyFn(_).bounds.withVariance(1))
    }

    def unapply(tp: Type)(implicit ctx: Context): Option[(List[Int], List[TypeBounds], Type)] = tp match {
      case app @ RefinedType(prefix, tpnme.hkApply) =>
        val cls = prefix.typeSymbol
        val variances = cls.typeParams.map(_.variance)
        val argBounds = prefix.argInfos.map(_.bounds)
        Some((variances, argBounds, app.refinedInfo.argInfo))
      case _ =>
        None
    }
  }

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
    def apply(tycon: TypeRef)(implicit ctx: Context) = {
      assert(tycon.isEtaExpandable)
      tycon.EtaExpand(tycon.typeParams)
    }

    def unapply(tp: Type)(implicit ctx: Context): Option[TypeRef] = {
      def argsAreForwarders(args: List[Type], n: Int): Boolean = args match {
        case Nil =>
          n == 0
        case TypeRef(RefinedThis(rt), sel) :: args1 =>
          rt.eq(tp) && sel == tpnme.hkArg(n - 1) && argsAreForwarders(args1, n - 1)
        case _ =>
          false
      }
      tp match {
        case TypeLambda(_, argBounds, AppliedType(fn: TypeRef, args))
        if argsAreForwarders(args, tp.typeParams.length) => Some(fn)
        //case TypeLambda(_, argBounds, AppliedType(fn: TypeRef, args)) =>
        //  println(i"eta expansion failed because args $args are not forwarders for ${tp.toString}")
        //  None
        //case TypeLambda(_, argBounds, _) =>
        //  println(i"eta expansion failed because body is not applied type")
        //  None
        case _ => None
      }
    }
  }

  /** Extractor for type application T[U_1, ..., U_n]. This is the refined type
   *
   *     T { type p_1 v_1= U_1; ...; type p_n v_n= U_n }
   *
   *  where v_i, p_i are the variances and names of the type parameters of T,
   *  If `T`'s class symbol is a lambda trait, follow the refined type with a
   *  projection
   *
   *      T { ... } # $Apply
   */
  object AppliedType {
    def apply(tp: Type, args: List[Type])(implicit ctx: Context): Type = tp.appliedTo(args)

    def unapply(tp: Type)(implicit ctx: Context): Option[(Type, List[Type])] = tp match {
      case TypeRef(prefix, tpnme.hkApply) => unapp(prefix)
      case _ =>
        unapp(tp) match {
          case Some((tycon: TypeRef, _)) if tycon.symbol.isLambdaTrait =>
            // We are seeing part of a lambda abstraction, not an applied type
            None
          case x => x
        }
    }

    private def unapp(tp: Type)(implicit ctx: Context): Option[(Type, List[Type])] = tp match {
      case _: RefinedType =>
        val tparams = tp.classSymbol.typeParams
        if (tparams.isEmpty) None
        else {
          val argBuf = new mutable.ListBuffer[Type]
          def stripArgs(tp: Type, n: Int): Type =
            if (n == 0) tp
            else tp match {
              case tp @ RefinedType(parent, pname) if pname == tparams(n - 1).name =>
                val res = stripArgs(parent, n - 1)
                if (res.exists) argBuf += tp.refinedInfo.argInfo
                res
              case _ =>
                NoType
            }
          val res = stripArgs(tp, tparams.length)
          if (res.exists) Some((res, argBuf.toList)) else None
        }
      case _ => None
    }
  }

   /** Adapt all arguments to possible higher-kinded type parameters using adaptIfHK
   */
  def adaptArgs(tparams: List[Symbol], args: List[Type])(implicit ctx: Context): List[Type] =
    if (tparams.isEmpty) args
    else args.zipWithConserve(tparams)((arg, tparam) => arg.adaptIfHK(tparam.infoOrCompleter))

  def argRefs(rt: RefinedType, n: Int)(implicit ctx: Context) =
    List.range(0, n).map(i => RefinedThis(rt).select(tpnme.hkArg(i)))
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
   *  any type parameter that is-rebound by the refinement. "Re-bind" means:
   *  The refinement contains a TypeAlias for the type parameter, or
   *  it introduces bounds for the type parameter, and we are not in the
   *  special case of a type Lambda, where a LambdaTrait gets refined
   *  with the bounds on its hk args. See `LambdaAbstract`, where these
   *  types get introduced, and see `isBoundedLambda` below for the test.
   */
  final def typeParams(implicit ctx: Context): List[TypeSymbol] = /*>|>*/ track("typeParams") /*<|<*/ {
    self match {
      case self: ClassInfo =>
        self.cls.typeParams
      case self: TypeRef =>
        val tsym = self.symbol
        if (tsym.isClass) tsym.typeParams
        else if (tsym.isAliasType) self.underlying.typeParams
        else if (tsym.isCompleting)
          // We are facing a problem when computing the type parameters of an uncompleted
          // abstract type. We can't access the bounds of the symbol yet because that
          // would cause a cause a cyclic reference. So we return `Nil` instead
          // and try to make up for it later. The acrobatics in Scala2Unpicker#readType
          // for reading a TypeRef show what's neeed.
          Nil
        else tsym.info.typeParams
      case self: RefinedType =>
        val hkParams = self.hkTypeParams
        if (hkParams.nonEmpty) hkParams
        else self.parent.typeParams.filterNot(_.name == self.refinedName)
      case self: SingletonType =>
        Nil
      case self: TypeProxy =>
        self.underlying.typeParams
      case _ =>
        Nil
    }
  }

  /** The higherkinded type parameters in case this is a type lambda
   *
   *     [X1, ..., Xn] -> T
   *
   *  These are the parameters of the underlying lambda class.
   *  Returns `Nil` for all other types.
   */
  final def hkTypeParams(implicit ctx: Context): List[TypeSymbol] =
    self.LambdaTrait.typeParams

  /** The Lambda trait underlying a type lambda */
  def LambdaTrait(implicit ctx: Context): Symbol = self.stripTypeVar match {
    case RefinedType(parent, tpnme.hkApply) =>
      val sym = self.classSymbol
      if (sym.isLambdaTrait) sym else NoSymbol
    case TypeBounds(lo, hi) => hi.LambdaTrait
    case _ => NoSymbol
  }

  /** A type ref is eta expandable if it refers to a non-lambda class.
   *  In that case we can look for parameterized base types fo the type
   *  to eta expand them.
   */
  def isEtaExpandable(implicit ctx: Context) = self match {
    case self: TypeRef => self.symbol.isClass && !self.name.isLambdaTraitName
    case _ => false
  }

  /** Lambda abstract `self` with given type parameters. Examples:
   *
   *      type T[X] = U        becomes    type T = [X] -> U
   *      type T[X] >: L <: U  becomes    type T >: L <: ([X] -> _ <: U)
   */
  def LambdaAbstract(tparams: List[Symbol])(implicit ctx: Context): Type = {
    def internalize[T <: Type](tp: T) =
      (rt: RefinedType) =>
        new ctx.SafeSubstMap(tparams, argRefs(rt, tparams.length))
          .apply(tp).asInstanceOf[T]
    def expand(tp: Type) = {
      TypeLambda(
        tparams.map(_.variance),
        tparams.map(tparam => internalize(self.memberInfo(tparam).bounds)),
        internalize(tp))
    }
    self match {
      case self: TypeAlias =>
        self.derivedTypeAlias(expand(self.alias))
      case self @ TypeBounds(lo, hi) =>
        self.derivedTypeBounds(lo, expand(TypeBounds.upper(hi)))
      case _ => expand(self)
    }
  }
/*
  def betaReduce(implicit ctx: Context): Type = self.stripTypeVar match {
    case TypeRef(prefix, tpnme.hkApply) =>
      prefix.betaReduce
    case self @ RefinedType(parent, tpnme.hkArg) if parent.isTypeLambda =>
      HKApplication(parent, self.refinedInfo.dropAlias)
  }
*/
  /** Adapt argument A to type parameter P in the case P is higher-kinded.
   *  This means:
   *  (1) Make sure that A is a type lambda, if necessary by eta-expanding it.
   *  (2) Make sure the variances of the type lambda
   *  agrees with variances of corresponding higherkinded type parameters. Example:
   *
   *     class Companion[+CC[X]]
   *     Companion[List]
   *
   *  with adaptArgs, this will expand to
   *
   *     Companion[[X] => List[X]]
   *
   *  instead of
   *
   *      Companion[[+X] => List[X]]
   *
   *  even though `List` is covariant. This adaptation is necessary to ignore conflicting
   *  variances in overriding members that have types of hk-type parameters such as `Companion[GenTraversable]`
   *  or `Companion[ListBuffer]`. Without the adaptation we would end up with
   *
   *      Companion[[+X] => GenTraversable[X]]
   *      Companion[[X] => List[X]]
   *
   *  and the second is not a subtype of the first. So if we have overridding memebrs of the two
   *  types we get an error.
   */
  def adaptIfHK(bound: Type)(implicit ctx: Context): Type = {
    val boundLambda = bound.LambdaTrait
    val hkParams = boundLambda.typeParams
    if (hkParams.isEmpty) self
    else self match {
      case self: TypeRef if self.symbol.isClass && self.typeParams.length == hkParams.length =>
        EtaExpansion(self).adaptIfHK(bound)
      case _ =>
        def adaptArg(arg: Type): Type = arg match {
          case arg: TypeRef
          if arg.symbol.isLambdaTrait &&
             !arg.symbol.typeParams.corresponds(boundLambda.typeParams)(_.variance == _.variance) =>
            arg.prefix.select(boundLambda)
          case arg: RefinedType =>
            arg.derivedRefinedType(adaptArg(arg.parent), arg.refinedName, arg.refinedInfo)
          case _ =>
            arg
        }
        adaptArg(self)
    }
  }

  /*
  /** If type `self` is equal, aliased-to, or upperbounded-by a type of the form
   *  `LambdaXYZ { ... }`, the class symbol of that type, otherwise NoSymbol.
   *  symbol of that type, otherwise NoSymbol.
   *  @param forcing  if set, might force completion. If not, never forces
   *                  but returns NoSymbol when it would have to otherwise.
   */
  def LambdaClass(forcing: Boolean)(implicit ctx: Context): Symbol = track("LambdaClass") { self.stripTypeVar match {
    case self: TypeRef =>
      val sym = self.symbol
      if (sym.isLambdaTrait) sym
      else if (sym.isClass || sym.isCompleting && !forcing) NoSymbol
      else self.info.LambdaClass(forcing)
    case self: TypeProxy =>
      self.underlying.LambdaClass(forcing)
    case _ =>
      NoSymbol
  }}

  /** Is type `self` equal, aliased-to, or upperbounded-by a type of the form
   *  `LambdaXYZ { ... }`?
   */
  def isLambda(implicit ctx: Context): Boolean =
    LambdaClass(forcing = true).exists

  /** Same is `isLambda`, except that symbol denotations are not forced
   *  Symbols in completion count as not lambdas.
   */
  def isSafeLambda(implicit ctx: Context): Boolean =
    LambdaClass(forcing = false).exists

  /** Is type `self` a Lambda with all hk$i fields fully instantiated? */
  def isInstantiatedLambda(implicit ctx: Context): Boolean =
    isSafeLambda && typeParams.isEmpty
*/
  /** Is receiver type higher-kinded (i.e. of kind != "*")? */
  def isHK(implicit ctx: Context): Boolean = self.dealias match {
    case self: TypeRef => self.info.isHK
    case RefinedType(_, name) => name == tpnme.hkApply
    case TypeBounds(_, hi) => hi.isHK
    case _ => false
  }

  /** is receiver of the form T#$apply? */
  def isHKApply: Boolean = self match {
    case TypeRef(_, name) => name == tpnme.hkApply
    case _ => false
  }

  /** True if it can be determined without forcing that the class symbol
   *  of this application exists and is not a lambda trait.
   *  Equivalent to
   *
   *    self.classSymbol.exists && !self.classSymbol.isLambdaTrait
   *
   *  but without forcing anything.
   */
  def classNotLambda(implicit ctx: Context): Boolean = self.stripTypeVar match {
    case self: RefinedType =>
      self.parent.classNotLambda
    case self: TypeRef =>
      self.denot.exists && {
        val sym = self.symbol
        if (sym.isClass) !sym.isLambdaTrait
        else sym.isCompleted && self.info.isAlias && self.info.bounds.hi.classNotLambda
      }
    case _ =>
      false
  }

  /** Encode the type resulting from applying this type to given arguments */
  final def appliedTo(args: List[Type])(implicit ctx: Context): Type = /*>|>*/ track("appliedTo") /*<|<*/ {
    def substHkArgs = new TypeMap {
      def apply(tp: Type): Type = tp match {
        case TypeRef(RefinedThis(rt), name) if rt.eq(self) && name.isHkArgName =>
          args(name.hkArgIndex)
        case _ =>
          mapOver(tp)
      }
    }
    if (args.isEmpty || ctx.erasedTypes) self
    else self.stripTypeVar match {
      case EtaExpansion(self1) =>
        self1.appliedTo(args)
      case TypeLambda(_, _, body) if !args.exists(_.isInstanceOf[TypeBounds]) =>
        substHkArgs(body)
      case self: PolyType =>
        self.instantiate(args)
      case _ =>
        appliedTo(args, typeParams)
    }
  }

  def appliedTo(args: List[Type], typParams: List[TypeSymbol])(implicit ctx: Context): Type = {
    def matchParams(t: Type, tparams: List[TypeSymbol], args: List[Type])(implicit ctx: Context): Type = args match {
      case arg :: args1 =>
        try {
          val tparam :: tparams1 = tparams
          matchParams(RefinedType(t, tparam.name, arg.toBounds(tparam)), tparams1, args1)
        } catch {
          case ex: MatchError =>
            println(s"applied type mismatch: $self $args, typeParams = $typParams") // !!! DEBUG
            //println(s"precomplete decls = ${self.typeSymbol.unforcedDecls.toList.map(_.denot).mkString("\n  ")}")
            throw ex
        }
      case nil => t
    }
    assert(args.nonEmpty)
    matchParams(self, typParams, args) match {
      case refined @ RefinedType(_, pname) if pname.isHkArgName =>
        TypeRef(refined, tpnme.hkApply)
      case refined =>
        refined
    }
  }


  final def appliedTo(arg: Type)(implicit ctx: Context): Type = appliedTo(arg :: Nil)
  final def appliedTo(arg1: Type, arg2: Type)(implicit ctx: Context): Type = appliedTo(arg1 :: arg2 :: Nil)

  final def safeAppliedTo(args: List[Type])(implicit ctx: Context) = {
    val safeTypeParams = self match {
      case self: TypeRef if !self.symbol.isClass && self.symbol.isCompleting =>
        // This happens when unpickling e.g. scala$collection$generic$GenMapFactory$$CC
        ctx.warning(i"encountered F-bounded higher-kinded type parameters for ${self.symbol}; assuming they are invariant")
        defn.LambdaTrait(args map alwaysZero).typeParams
      case _ =>
        typeParams
    }
    appliedTo(args, safeTypeParams)
  }
/*
  /** Simplify a fully instantiated type of the form `LambdaX{... type Apply = T } # Apply` to `T`.
   */
  def simplifyApply(implicit ctx: Context): Type = self match {
    case self @ TypeRef(prefix, tpnme.hkApply) if prefix.isInstantiatedLambda =>
      prefix.member(tpnme.hkApply).info match {
        case TypeAlias(alias) => alias
        case _ => self
      }
    case _ => self
  }
*/

  /** Turn this type, which is used as an argument for
   *  type parameter `tparam`, into a TypeBounds RHS
   */
  final def toBounds(tparam: Symbol)(implicit ctx: Context): TypeBounds = self match {
    case self: TypeBounds => // this can happen for wildcard args
      self
    case _ =>
      val v = tparam.variance
      if (v > 0 && !(tparam is Local) && !(tparam is ExpandedTypeParam)) TypeBounds.upper(self)
      else if (v < 0 && !(tparam is Local) && !(tparam is ExpandedTypeParam)) TypeBounds.lower(self)
      else TypeAlias(self, v)
  }

  /** The type arguments of this type's base type instance wrt.`base`.
   *  Existential types in arguments are returned as TypeBounds instances.
   */
  final def baseArgInfos(base: Symbol)(implicit ctx: Context): List[Type] =
    if (self derivesFrom base)
      base.typeParams map (param => self.member(param.name).info.argInfo)
    else
      Nil

  /** The type arguments of this type's base type instance wrt.`base`.
   *  Existential types in arguments are disallowed.
   */
  final def baseArgTypes(base: Symbol)(implicit ctx: Context): List[Type] =
    baseArgInfos(base) mapConserve noBounds

  /** The type arguments of this type's base type instance wrt.`base`.
   *  Existential types in arguments are approximated by their lower bound.
   */
  final def baseArgTypesLo(base: Symbol)(implicit ctx: Context): List[Type] =
    baseArgInfos(base) mapConserve boundsToLo

  /** The type arguments of this type's base type instance wrt.`base`.
   *  Existential types in arguments are approximated by their upper bound.
   */
  final def baseArgTypesHi(base: Symbol)(implicit ctx: Context): List[Type] =
    baseArgInfos(base) mapConserve boundsToHi

  /** The first type argument of the base type instance wrt `base` of this type */
  final def firstBaseArgInfo(base: Symbol)(implicit ctx: Context): Type = base.typeParams match {
    case param :: _ if self derivesFrom base =>
      self.member(param.name).info.argInfo
    case _ =>
      NoType
  }

  /** The base type including all type arguments and applicable refinements
   *  of this type. Refinements are applicable if they refine a member of
   *  the parent type which furthermore is not a name-mangled type parameter.
   *  Existential types in arguments are returned as TypeBounds instances.
   */
  final def baseTypeWithArgs(base: Symbol)(implicit ctx: Context): Type = ctx.traceIndented(s"btwa ${self.show} wrt $base", core, show = true) {
    def default = self.baseTypeRef(base).appliedTo(baseArgInfos(base))
    self match {
      case tp: TypeRef =>
        tp.info match {
          case TypeBounds(_, hi) => hi.baseTypeWithArgs(base)
          case _ => default
        }
      case tp @ RefinedType(parent, name) if !tp.member(name).symbol.is(ExpandedTypeParam) =>
        tp.wrapIfMember(parent.baseTypeWithArgs(base))
      case tp: TermRef =>
        tp.underlying.baseTypeWithArgs(base)
      case AndType(tp1, tp2) =>
        tp1.baseTypeWithArgs(base) & tp2.baseTypeWithArgs(base)
      case OrType(tp1, tp2) =>
        tp1.baseTypeWithArgs(base) | tp2.baseTypeWithArgs(base)
      case _ =>
        default
    }
  }

  /** Translate a type of the form From[T] to To[T], keep other types as they are.
   *  `from` and `to` must be static classes, both with one type parameter, and the same variance.
   *  Do the same for by name types => From[T] and => To[T]
   */
  def translateParameterized(from: ClassSymbol, to: ClassSymbol)(implicit ctx: Context): Type = self match {
    case self @ ExprType(tp) =>
      self.derivedExprType(tp.translateParameterized(from, to))
    case _ =>
      if (self.derivesFrom(from))
        if (ctx.erasedTypes) to.typeRef
        else RefinedType(to.typeRef, to.typeParams.head.name, self.member(from.typeParams.head.name).info)
      else self
  }

  /** If this is repeated parameter type, its underlying Seq type,
   *  or, if isJava is true, Array type, else the type itself.
   */
  def underlyingIfRepeated(isJava: Boolean)(implicit ctx: Context): Type =
    if (self.isRepeatedParam) {
      val seqClass = if (isJava) defn.ArrayClass else defn.SeqClass
      translateParameterized(defn.RepeatedParamClass, seqClass)
    }
    else self

  /** If this is an encoding of a (partially) applied type, return its arguments,
   *  otherwise return Nil.
   *  Existential types in arguments are returned as TypeBounds instances.
   */
  final def argInfos(implicit ctx: Context): List[Type] = self match {
    case AppliedType(tycon, args) => args
    case _ => Nil
  }

  /** Argument types where existential types in arguments are disallowed */
  def argTypes(implicit ctx: Context) = argInfos mapConserve noBounds

  /** Argument types where existential types in arguments are approximated by their lower bound */
  def argTypesLo(implicit ctx: Context) = argInfos mapConserve boundsToLo

  /** Argument types where existential types in arguments are approximated by their upper bound  */
  def argTypesHi(implicit ctx: Context) = argInfos mapConserve boundsToHi

  /** The core type without any type arguments.
   *  @param `typeArgs` must be the type arguments of this type.
   */
  final def withoutArgs(typeArgs: List[Type]): Type = typeArgs match {
    case _ :: typeArgs1 =>
      val RefinedType(tycon, _) = self
      tycon.withoutArgs(typeArgs1)
    case nil =>
      self
  }

  final def typeConstructor(implicit ctx: Context): Type = self.stripTypeVar match {
    case AppliedType(tycon, _) => tycon
    case self => self
  }

  /** If this is the image of a type argument; recover the type argument,
   *  otherwise NoType.
   */
  final def argInfo(implicit ctx: Context): Type = self match {
    case self: TypeAlias => self.alias
    case self: TypeBounds => self
    case _ => NoType
  }

  def dropAlias(implicit ctx: Context): Type = self match {
    case TypeAlias(alias) => alias
    case _ => self
  }

  /** The element type of a sequence or array */
  def elemType(implicit ctx: Context): Type = self match {
    case defn.ArrayOf(elemtp) => elemtp
    case JavaArrayType(elemtp) => elemtp
    case _ => firstBaseArgInfo(defn.SeqClass)
  }

  def containsRefinedThis(target: Type)(implicit ctx: Context): Boolean = {
    def recur(tp: Type): Boolean = tp.stripTypeVar match {
      case RefinedThis(tp) =>
        tp eq target
      case tp: NamedType =>
        if (tp.symbol.isClass) !tp.symbol.isStatic && recur(tp.prefix)
        else tp.info match {
          case TypeAlias(alias) => recur(alias)
          case _ => recur(tp.prefix)
        }
      case tp: RefinedType =>
        recur(tp.refinedInfo) || recur(tp.parent)
      case tp: TypeBounds =>
        recur(tp.lo) || recur(tp.hi)
      case tp: AnnotatedType =>
        recur(tp.underlying)
      case tp: AndOrType =>
        recur(tp.tp1) || recur(tp.tp2)
      case _ =>
        false
    }
    recur(self)
  }
/*
  /** The typed lambda abstraction of this type `T` relative to `boundSyms`.
   *  This is:
   *
   *      LambdaXYZ{ bounds }{ type Apply = toHK(T) }
   *
   *  where
   *   - XYZ reflects the variances of the bound symbols,
   *   - `bounds` consists of type declarations `type hk$i >: toHK(L) <: toHK(U),
   *     one for each type parameter in `T` with non-trivial bounds L,U.
   *   - `toHK` is a substitution that replaces every bound symbol sym_i by
   *     `this.hk$i`.
   *
   *  TypeBounds are lambda abstracting by lambda abstracting their upper bound.
   *
   *  @param cycleParanoid   If `true` don't force denotation of a TypeRef unless
   *                         its name matches one of `boundSyms`. Needed to avoid cycles
   *                         involving F-boundes hk-types when reading Scala2 collection classes
   *                         with new hk-scheme.
   */
  def LambdaAbstract(boundSyms: List[Symbol], cycleParanoid: Boolean = false)(implicit ctx: Context): Type = {
    def expand(tp: Type): Type = {
      val lambda = defn.LambdaTrait(boundSyms.map(_.variance))
      def toHK(tp: Type) = (rt: RefinedType) => {
        val argRefs = boundSyms.indices.toList.map(i =>
          RefinedThis(rt).select(tpnme.hkArg(i)))
        val substituted =
          if (cycleParanoid) new ctx.SafeSubstMap(boundSyms, argRefs).apply(tp)
          else tp.subst(boundSyms, argRefs)
        substituted.bounds.withVariance(1)
      }
      val boundNames = new mutable.ListBuffer[Name]
      val boundss = new mutable.ListBuffer[TypeBounds]
      for (sym <- boundSyms) {
        val bounds = sym.info.bounds
        if (!(TypeBounds.empty frozen_<:< bounds)) {
          boundNames += sym.name
          boundss += bounds
        }
      }
      val lambdaWithBounds =
        RefinedType.make(lambda.typeRef, boundNames.toList, boundss.toList.map(toHK))
      RefinedType(lambdaWithBounds, tpnme.hkApply, toHK(tp))
    }
    self match {
      case self @ TypeBounds(lo, hi) =>
        self.derivedTypeBounds(lo, expand(TypeBounds.upper(hi)))
      case _ =>
        expand(self)
    }
  }
*/
  /** Convert a type constructor `TC` which has type parameters `T1, ..., Tn`
   *  in a context where type parameters `U1,...,Un` are expected to
   *
   *     LambdaXYZ { Apply = TC[hk$0, ..., hk$n] }
   *
   *  Here, XYZ corresponds to the variances of
   *   - `U1,...,Un` if the variances of `T1,...,Tn` are pairwise compatible with `U1,...,Un`,
   *   - `T1,...,Tn` otherwise.
   *  v1 is compatible with v2, if v1 = v2 or v2 is non-variant.
   */
  def EtaExpand(tparams: List[TypeSymbol])(implicit ctx: Context): Type = {
    val tparamsToUse = if (variancesConform(typeParams, tparams)) tparams else typeParams
    self.appliedTo(tparams map (_.typeRef)).LambdaAbstract(tparamsToUse)
      //.ensuring(res => res.EtaReduce =:= self, s"res = $res, core = ${res.EtaReduce}, self = $self, hc = ${res.hashCode}")
  }
/*
  /** Eta expand if `bound` is a higher-kinded type */
  def EtaExpandIfHK(bound: Type)(implicit ctx: Context): Type =
    if (bound.isHK && !isHK && self.typeSymbol.isClass && typeParams.nonEmpty) EtaExpand(bound.typeParams)
    else self
*/
  /** Eta expand the prefix in front of any refinements. */
  def EtaExpandCore(implicit ctx: Context): Type = self.stripTypeVar match {
    case self: RefinedType =>
      self.derivedRefinedType(self.parent.EtaExpandCore, self.refinedName, self.refinedInfo)
    case _ =>
      self.EtaExpand(self.typeParams)
  }
/*
  /** If `self` is a (potentially partially instantiated) eta expansion of type T, return T,
   *  otherwise NoType. More precisely if `self` is of the form
   *
   *    T { type $apply = U[T1, ..., Tn] }
   *
   *  where
   *
   *   - hk$0, ..., hk${m-1} are the type parameters of T
   *   - a sublist of the arguments Ti_k (k = 0,...,m_1) are of the form T{...}.this.hk$i_k
   *
   *  rewrite `self` to
   *
   *    U[T'1,...T'j]
   *
   *   where
   *
   *      T'j = _ >: Lj <: Uj   if j is in the i_k list defined above
   *                            where Lj and Uj are the bounds of hk$j mapped using `fromHK`.
   *          = fromHK(Tj)   otherwise.
   *
   *   `fromHK` is the function that replaces every occurrence of `<self>.this.hk$i` by the
   *   corresponding parameter reference in `U[T'1,...T'j]`
   */
  def EtaReduce(implicit ctx: Context): Type = {
    def etaCore(tp: Type, tparams: List[Symbol]): Type = tparams match {
      case Nil => tp
      case tparam :: otherParams =>
        tp match {
          case tp: RefinedType =>
            tp.refinedInfo match {
              case TypeAlias(TypeRef(RefinedThis(rt), rname))
              if (rname == tparam.name) && (rt eq self) =>
                // we have a binding T = Lambda$XYZ{...}.this.hk$i where hk$i names the current `tparam`.
                val pcore = etaCore(tp.parent, otherParams)
                val hkBounds = self.member(rname).info.bounds
                if (TypeBounds.empty frozen_<:< hkBounds) pcore
                else tp.derivedRefinedType(pcore, tp.refinedName, hkBounds)
              case _ =>
                val pcore = etaCore(tp.parent, tparams)
                if (pcore.exists) tp.derivedRefinedType(pcore, tp.refinedName, tp.refinedInfo)
                else NoType
            }
          case _ =>
            NoType
        }
    }
    // Map references `Lambda$XYZ{...}.this.hk$i to corresponding parameter references of the reduced core.
    def fromHK(reduced: Type) = reduced match {
      case reduced: RefinedType =>
        new TypeMap {
          def apply(tp: Type): Type = tp match {
            case TypeRef(RefinedThis(binder), name) if binder eq self =>
              assert(name.isHkArgName)
              RefinedThis(reduced).select(reduced.typeParams.apply(name.hkArgIndex))
            case _ =>
              mapOver(tp)
          }
        }.apply(reduced)
      case _ =>
        reduced
    }

    self match {
      case self @ RefinedType(parent, tpnme.hkApply) =>
        val lc = parent.LambdaClass(forcing = false)
        self.refinedInfo match {
          case TypeAlias(alias) if lc.exists =>
            fromHK(etaCore(alias, lc.typeParams.reverse))
          case _ => NoType
        }
      case _ => NoType
    }
  }

  /** Test whether this type has a base type of the form `B[T1, ..., Tn]` where
   *  the type parameters of `B` match one-by-one the variances of `tparams`,
   *  and where the lambda abstracted type
   *
   *     LambdaXYZ { type Apply = B[hk$0, ..., hk${n-1}] }
   *               { type hk$0 = T1; ...; type hk${n-1} = Tn }
   *
   *  satisfies predicate `p`. Try base types in the order of their occurrence in `baseClasses`.
   *  A type parameter matches a variance V if it has V as its variance or if V == 0.
   *  @param classBounds  A hint to bound the search. Only types that derive from one of the
   *                      classes in classBounds are considered.
   */
  def testLifted(tparams: List[Symbol], p: Type => Boolean, classBounds: List[ClassSymbol] = Nil)(implicit ctx: Context): Boolean = {
    def recur(bcs: List[ClassSymbol]): Boolean = bcs match {
      case bc :: bcs1 =>
        val baseRef = self.baseTypeRef(bc)
        def variancesMatch(param1: Symbol, param2: Symbol) =
          param2.variance == param2.variance || param2.variance == 0
        (classBounds.exists(bc.derivesFrom) &&
         baseRef.typeParams.corresponds(tparams)(variancesMatch) &&
         p(baseRef.appliedTo(self.baseArgInfos(bc)))
         ||
         recur(bcs1))
      case nil =>
        false
    }
    classBounds.nonEmpty && recur(self.baseClasses)
  }
  */
}
