package dotty.tools
package dotc
package core

import util.common._
import Symbols._
import Flags._
import Names._
import StdNames._, NameOps._
import NameKinds.{ShadowedName, SkolemName}
import Scopes._
import Constants._
import Contexts._
import Annotations._
import SymDenotations._
import Decorators._
import Denotations._
import Periods._
import util.Positions.{Position, NoPosition}
import util.Stats._
import util.{DotClass, SimpleMap}
import reporting.diagnostic.Message
import reporting.diagnostic.messages.CyclicReferenceInvolving
import ast.tpd._
import ast.TreeTypeMap
import printing.Texts._
import ast.untpd
import dotty.tools.dotc.transform.Erasure
import printing.Printer
import Hashable._
import Uniques._
import collection.{mutable, Seq, breakOut}
import config.Config
import annotation.tailrec
import Flags.FlagSet
import language.implicitConversions
import scala.util.hashing.{ MurmurHash3 => hashing }
import config.Printers.{core, typr, cyclicErrors}
import java.lang.ref.WeakReference

object Types {

  @sharable private var nextId = 0

  implicit def eqType: Eq[Type, Type] = Eq

  /** Main class representing types.
   *
   *  The principal subclasses and sub-objects are as follows:
   *
   *  ```none
   *  Type -+- ProxyType --+- NamedType ----+--- TypeRef
   *        |              |                 \
   *        |              +- SingletonType-+-+- TermRef
   *        |              |                |
   *        |              |                +--- ThisType
   *        |              |                +--- SuperType
   *        |              |                +--- ConstantType
   *        |              |                +--- TermParamRef
   *        |              |                +----RecThis
   *        |              |                +--- SkolemType
   *        |              +- TypeParamRef
   *        |              +- RefinedOrRecType -+-- RefinedType
   *        |              |                   -+-- RecType
   *        |              +- HKApply
   *        |              +- TypeBounds
   *        |              +- ExprType
   *        |              +- AnnotatedType
   *        |              +- TypeVar
   *        |              +- HKTypeLambda
   *        |
   *        +- GroundType -+- AndType
   *                       +- OrType
   *                       +- MethodOrPoly ---+-- PolyType
   *                                          +-- MethodType ---+- ImplicitMethodType
   *                       |                                    +- JavaMethodType
   *                       +- ClassInfo
   *                       |
   *                       +- NoType
   *                       +- NoPrefix
   *                       +- ErrorType
   *                       +- WildcardType
   *  ```
   *
   *  Note: please keep in sync with copy in `docs/docs/internals/type-system.md`.
   */
  abstract class Type extends DotClass with Hashable with printing.Showable {

// ----- Tests -----------------------------------------------------

    // debug only: a unique identifier for a type
    val uniqId = {
      nextId = nextId + 1
//      if (nextId == 19555)
//        println("foo")
      nextId
    }

    /** Is this type different from NoType? */
    def exists: Boolean = true

    /** This type, if it exists, otherwise `that` type */
    def orElse(that: => Type) = if (exists) this else that

    /** Is this type a value type? */
    final def isValueType: Boolean = this.isInstanceOf[ValueType]

    /** Is the is value type or type lambda? */
    final def isValueTypeOrLambda: Boolean = isValueType || this.isInstanceOf[TypeLambda]

    /** Does this type denote a stable reference (i.e. singleton type)? */
    final def isStable(implicit ctx: Context): Boolean = stripTypeVar match {
      case tp: TermRef => tp.termSymbol.isStable && tp.prefix.isStable || tp.info.isStable
      case _: SingletonType | NoPrefix => true
      case tp: RefinedOrRecType => tp.parent.isStable
      case tp: ExprType => tp.resultType.isStable
      case _ => false
    }

    /** Is this type a (possibly refined or applied or aliased) type reference
     *  to the given type symbol?
     *  @sym  The symbol to compare to. It must be a class symbol or abstract type.
     *        It makes no sense for it to be an alias type because isRef would always
     *        return false in that case.
     */
    def isRef(sym: Symbol)(implicit ctx: Context): Boolean = stripAnnots.stripTypeVar match {
      case this1: TypeRef =>
        this1.info match { // see comment in Namer#typeDefSig
          case TypeAlias(tp) =>
            assert((tp ne this) && (tp ne this1), s"$tp / $this")
            tp.isRef(sym)
          case _ => this1.symbol eq sym
        }
      case this1: RefinedOrRecType => this1.parent.isRef(sym)
      case this1: AppliedType =>
        val this2 = this1.dealias
        if (this2 ne this1) this2.isRef(sym)
        else this1.underlying.isRef(sym)
      case this1: HKApply =>
        val this2 = this1.dealias
        if (this2 ne this1) this2.isRef(sym)
        else this1.underlying.isRef(sym)
      case _ => false
    }

    /** Is this type a (neither aliased nor applied) reference to class `sym`? */
    def isDirectRef(sym: Symbol)(implicit ctx: Context): Boolean = stripTypeVar match {
      case this1: TypeRef =>
        this1.name == sym.name && // avoid forcing info if names differ
        (this1.symbol eq sym)
      case _ =>
        false
    }

    def isInfixType(implicit ctx: Context): Boolean = this match {
      case TypeApplications.AppliedType(tycon, args) =>
        args.length == 2 &&
          !Character.isUnicodeIdentifierStart(tycon.typeSymbol.name.toString.head)
          // TODO: Once we use the 2.12 stdlib, also check the @showAsInfix annotation
      case _ => false
    }

    /** Does this type refer exactly to class symbol `sym`, instead of to a subclass of `sym`?
     *  Implemented like `isRef`, but follows more types: all type proxies as well as and- and or-types
     */
    private[Types] def isTightPrefix(sym: Symbol)(implicit ctx: Context): Boolean = stripTypeVar match {
      case tp: NamedType => tp.info.isTightPrefix(sym)
      case tp: ClassInfo => tp.cls eq sym
      case tp: Types.ThisType => tp.cls eq sym
      case tp: TypeProxy => tp.underlying.isTightPrefix(sym)
      case tp: AndType => tp.tp1.isTightPrefix(sym) && tp.tp2.isTightPrefix(sym)
      case tp: OrType => tp.tp1.isTightPrefix(sym) || tp.tp2.isTightPrefix(sym)
      case _ => false
    }

    /** Is this type an instance of a non-bottom subclass of the given class `cls`? */
    final def derivesFrom(cls: Symbol)(implicit ctx: Context): Boolean = {
      def loop(tp: Type): Boolean = tp match {
        case tp: TypeRef =>
          val sym = tp.symbol
          if (sym.isClass) sym.derivesFrom(cls) else loop(tp.superType): @tailrec
        case tp: TypeProxy =>
          loop(tp.underlying): @tailrec
        case tp: AndType =>
          loop(tp.tp1) || loop(tp.tp2): @tailrec
        case tp: OrType =>
          loop(tp.tp1) && loop(tp.tp2): @tailrec
        case tp: JavaArrayType =>
          cls == defn.ObjectClass
        case _ =>
          false
      }
      loop(this)
    }

    /** Returns true if the type is a phantom type
     *   - true if XYZ extends scala.Phantom and this type is upper bounded XYZ.Any
     *   - false otherwise
     */
    final def isPhantom(implicit ctx: Context): Boolean = phantomLatticeType.exists

    /** Returns the top type of the lattice
     *   - XYX.Any if XYZ extends scala.Phantom and this type is upper bounded XYZ.Any
     *   - scala.Any otherwise
     */
    final def topType(implicit ctx: Context): Type = {
      val lattice = phantomLatticeType
      if (lattice.exists) lattice.select(tpnme.Any)
      else defn.AnyType
    }

    /** Returns the bottom type of the lattice
     *   - XYZ.Nothing if XYZ extends scala.Phantom and this type is upper bounded XYZ.Any
     *   - scala.Nothing otherwise
     */
    final def bottomType(implicit ctx: Context): Type = {
      val lattice = phantomLatticeType
      if (lattice.exists) lattice.select(tpnme.Nothing)
      else defn.NothingType
    }

    /** Is this type exactly Nothing (no vars, aliases, refinements etc allowed)? */
    def isBottomType(implicit ctx: Context): Boolean = this match {
      case tp: TypeRef =>
        val sym = tp.symbol
        (sym eq defn.NothingClass) || (sym eq defn.Phantom_NothingClass)
      case _ => false
    }

      /** Is this type exactly Any (no vars, aliases, refinements etc allowed)? */
    def isTopType(implicit ctx: Context): Boolean = this match {
      case tp: TypeRef =>
        val sym = tp.symbol
        (sym eq defn.AnyClass) || (sym eq defn.Phantom_AnyClass)
      case _ => false
    }

    /** Returns the type of the phantom lattice (i.e. the prefix of the phantom type)
      *   - XYZ if XYZ extends scala.Phantom and this type is upper bounded XYZ.Any
      *   - NoType otherwise
      */
    private final def phantomLatticeType(implicit ctx: Context): Type = widen match {
      case tp: ClassInfo if defn.isPhantomTerminalClass(tp.classSymbol) => tp.prefix
      case tp: TypeProxy if tp.superType ne this => tp.underlying.phantomLatticeType // ??? guard needed ???
      case tp: AndOrType => tp.tp1.phantomLatticeType
      case _ => NoType
    }

    /** Is this type guaranteed not to have `null` as a value? */
    final def isNotNull(implicit ctx: Context): Boolean = this match {
      case tp: ConstantType => tp.value.value != null
      case tp: ClassInfo => !tp.cls.isNullableClass && tp.cls != defn.NothingClass
      case tp: TypeBounds => tp.lo.isNotNull
      case tp: TypeProxy => tp.underlying.isNotNull
      case AndType(tp1, tp2) => tp1.isNotNull || tp2.isNotNull
      case OrType(tp1, tp2) => tp1.isNotNull && tp2.isNotNull
      case _ => false
    }

    /** Is this type produced as a repair for an error? */
    final def isError(implicit ctx: Context): Boolean = stripTypeVar match {
      case _: ErrorType => true
      case tp => (tp.typeSymbol is Erroneous) || (tp.termSymbol is Erroneous)
    }

    /** Is some part of this type produced as a repair for an error? */
    final def isErroneous(implicit ctx: Context): Boolean = existsPart(_.isError, forceLazy = false)

    /** Does the type carry an annotation that is an instance of `cls`? */
    @tailrec final def hasAnnotation(cls: ClassSymbol)(implicit ctx: Context): Boolean = stripTypeVar match {
      case AnnotatedType(tp, annot) => (annot matches cls) || (tp hasAnnotation cls)
      case _ => false
    }

    /** Does this type occur as a part of type `that`? */
    final def occursIn(that: Type)(implicit ctx: Context): Boolean =
      that existsPart (this == _)

    /** Is this a type of a repeated parameter? */
    def isRepeatedParam(implicit ctx: Context): Boolean =
      typeSymbol eq defn.RepeatedParamClass

    /** Is this the type of a method that has a repeated parameter type as
     *  last parameter type?
     */
    def isVarArgsMethod(implicit ctx: Context): Boolean = stripPoly match {
      case mt: MethodType => mt.paramInfos.nonEmpty && mt.paramInfos.last.isRepeatedParam
      case _ => false
    }

    /** Is this the type of a method with a leading empty parameter list?
     */
    def isNullaryMethod(implicit ctx: Context): Boolean = stripPoly match {
      case MethodType(Nil) => true
      case _ => false
    }

    /** Is this an alias TypeBounds? */
    final def isAlias: Boolean = this.isInstanceOf[TypeAlias]

// ----- Higher-order combinators -----------------------------------

    /** Returns true if there is a part of this type that satisfies predicate `p`.
     */
    final def existsPart(p: Type => Boolean, forceLazy: Boolean = true)(implicit ctx: Context): Boolean =
      new ExistsAccumulator(p, forceLazy).apply(false, this)

    /** Returns true if all parts of this type satisfy predicate `p`.
     */
    final def forallParts(p: Type => Boolean)(implicit ctx: Context): Boolean =
      !existsPart(!p(_))

    /** Performs operation on all parts of this type */
    final def foreachPart(p: Type => Unit, stopAtStatic: Boolean = false)(implicit ctx: Context): Unit =
      new ForeachAccumulator(p, stopAtStatic).apply((), this)

    /** The parts of this type which are type or term refs */
    final def namedParts(implicit ctx: Context): collection.Set[NamedType] =
      namedPartsWith(alwaysTrue)

    /** The parts of this type which are type or term refs and which
     *  satisfy predicate `p`.
     *
     *  @param p                   The predicate to satisfy
     *  @param excludeLowerBounds  If set to true, the lower bounds of abstract
     *                             types will be ignored.
     */
    def namedPartsWith(p: NamedType => Boolean, excludeLowerBounds: Boolean = false)
      (implicit ctx: Context): collection.Set[NamedType] =
      new NamedPartsAccumulator(p, excludeLowerBounds).apply(mutable.LinkedHashSet(), this)

    /** Map function `f` over elements of an AndType, rebuilding with function `g` */
    def mapReduceAnd[T](f: Type => T)(g: (T, T) => T)(implicit ctx: Context): T = stripTypeVar match {
      case AndType(tp1, tp2) => g(tp1.mapReduceAnd(f)(g), tp2.mapReduceAnd(f)(g))
      case _ => f(this)
    }

    /** Map function `f` over elements of an OrType, rebuilding with function `g` */
    final def mapReduceOr[T](f: Type => T)(g: (T, T) => T)(implicit ctx: Context): T = stripTypeVar match {
      case OrType(tp1, tp2) => g(tp1.mapReduceOr(f)(g), tp2.mapReduceOr(f)(g))
      case _ => f(this)
    }

// ----- Associated symbols ----------------------------------------------

    /** The type symbol associated with the type */
    @tailrec final def typeSymbol(implicit ctx: Context): Symbol = this match {
      case tp: TypeRef => tp.symbol
      case tp: ClassInfo => tp.cls
//    case ThisType(cls) => cls // needed?
      case tp: SingletonType => NoSymbol
      case tp: TypeProxy => tp.underlying.typeSymbol
      case _ => NoSymbol
    }

    /** The least class or trait of which this type is a subtype or parameterized
     *  instance, or NoSymbol if none exists (either because this type is not a
     *  value type, or because superclasses are ambiguous).
     */
    final def classSymbol(implicit ctx: Context): Symbol = this match {
      case ConstantType(constant) =>
        constant.tpe.classSymbol: @tailrec
      case tp: TypeRef =>
        val sym = tp.symbol
        if (sym.isClass) sym else tp.superType.classSymbol: @tailrec
      case tp: ClassInfo =>
        tp.cls
      case tp: SingletonType =>
        NoSymbol
      case tp: TypeProxy =>
        tp.underlying.classSymbol: @tailrec
      case AndType(l, r) =>
        val lsym = l.classSymbol
        val rsym = r.classSymbol
        if (lsym isSubClass rsym) lsym
        else if (rsym isSubClass lsym) rsym
        else NoSymbol
      case OrType(l, r) => // TODO does not conform to spec
        val lsym = l.classSymbol
        val rsym = r.classSymbol
        if (lsym isSubClass rsym) rsym
        else if (rsym isSubClass lsym) lsym
        else NoSymbol
      case _ =>
        NoSymbol
    }

    /** The least (wrt <:<) set of class symbols of which this type is a subtype
     */
    final def classSymbols(implicit ctx: Context): List[ClassSymbol] = this match {
      case tp: ClassInfo =>
        tp.cls :: Nil
      case tp: TypeRef =>
        val sym = tp.symbol
        if (sym.isClass) sym.asClass :: Nil else tp.superType.classSymbols: @tailrec
      case tp: TypeProxy =>
        tp.underlying.classSymbols: @tailrec
      case AndType(l, r) =>
        l.classSymbols union r.classSymbols
      case OrType(l, r) =>
        l.classSymbols intersect r.classSymbols // TODO does not conform to spec
      case _ =>
        Nil
    }

    /** The term symbol associated with the type */
    @tailrec final def termSymbol(implicit ctx: Context): Symbol = this match {
      case tp: TermRef => tp.symbol
      case tp: TypeProxy => tp.underlying.termSymbol
      case _ => NoSymbol
    }

    /** The base classes of this type as determined by ClassDenotation
     *  in linearization order, with the class itself as first element.
     *  Inherited by all type proxies. Overridden for And and Or types.
     *  `Nil` for all other types.
     */
    def baseClasses(implicit ctx: Context): List[ClassSymbol] = track("baseClasses") {
      this match {
        case tp: TypeProxy =>
          tp.underlying.baseClasses
        case tp: ClassInfo =>
          tp.cls.baseClasses
        case _ => Nil
      }
    }

// ----- Member access -------------------------------------------------

    /** The scope of all declarations of this type.
     *  Defined by ClassInfo, inherited by type proxies.
     *  Empty scope for all other types.
     */
    @tailrec final def decls(implicit ctx: Context): Scope = this match {
      case tp: ClassInfo =>
        tp.decls
      case tp: TypeProxy =>
        tp.underlying.decls: @tailrec
      case _ =>
        EmptyScope
    }

    /** A denotation containing the declaration(s) in this type with the given name.
     *  The result is either a SymDenotation or a MultiDenotation of SymDenotations.
     *  The info(s) are the original symbol infos, no translation takes place.
     */
    final def decl(name: Name)(implicit ctx: Context): Denotation = track("decl") {
      findDecl(name, EmptyFlags)
    }

    /** A denotation containing the non-private declaration(s) in this type with the given name */
    final def nonPrivateDecl(name: Name)(implicit ctx: Context): Denotation = track("nonPrivateDecl") {
      findDecl(name, Private)
    }

    /** A denotation containing the declaration(s) in this type with the given
     *  name, as seen from prefix type `pre`. Declarations that have a flag
     *  in `excluded` are omitted.
     */
    @tailrec final def findDecl(name: Name, excluded: FlagSet)(implicit ctx: Context): Denotation = this match {
      case tp: ClassInfo =>
        tp.decls.denotsNamed(name).filterExcluded(excluded).toDenot(NoPrefix)
      case tp: TypeProxy =>
        tp.underlying.findDecl(name, excluded)
      case err: ErrorType =>
        ctx.newErrorSymbol(classSymbol orElse defn.RootClass, name, err.msg)
      case _ =>
        NoDenotation
    }

    /** The member of this type with the given name  */
    final def member(name: Name)(implicit ctx: Context): Denotation = /*>|>*/ track("member") /*<|<*/ {
      memberExcluding(name, EmptyFlags)
    }

    /** The non-private member of this type with the given name. */
    final def nonPrivateMember(name: Name)(implicit ctx: Context): Denotation = track("nonPrivateMember") {
      memberExcluding(name, Flags.Private)
    }

    final def memberExcluding(name: Name, excluding: FlagSet)(implicit ctx: Context): Denotation = {
      // We need a valid prefix for `asSeenFrom`
      val pre = this match {
        case tp: ClassInfo =>
          tp.typeRef // @!!! appliedRef
        case _ =>
          widenIfUnstable
      }
      findMember(name, pre, excluding)
    }

    /** Find member of this type with given name and
     *  produce a denotation that contains the type of the member
     *  as seen from given prefix `pre`. Exclude all members that have
     *  flags in `excluded` from consideration.
     */
    final def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Denotation = {
      @tailrec def go(tp: Type): Denotation = tp match {
        case tp: TermRef =>
          go (tp.underlying match {
            case mt: MethodType
            if mt.paramInfos.isEmpty && (tp.symbol is Stable) => mt.resultType
            case tp1 => tp1
          })
        case tp: TypeRef =>
          tp.denot.findMember(name, pre, excluded)
        case tp: AppliedType =>
          goApplied(tp)
        case tp: ThisType =>
          goThis(tp)
        case tp: RefinedType =>
          if (name eq tp.refinedName) goRefined(tp) else go(tp.parent)
        case tp: RecType =>
          goRec(tp)
        case tp: TypeParamRef =>
          goParam(tp)
        case tp: SuperType =>
          goSuper(tp)
        case tp: HKApply =>
          goHKApply(tp)
        case tp: TypeProxy =>
          go(tp.underlying)
        case tp: ClassInfo =>
          tp.cls.findMember(name, pre, excluded)
        case AndType(l, r) =>
          goAnd(l, r)
        case tp: OrType =>
          // we need to keep the invariant that `pre <: tp`. Branch `union-types-narrow-prefix`
          // achieved that by narrowing `pre` to each alternative, but it led to merge errors in
          // lots of places. The present strategy is instead of widen `tp` using `join` to be a
          // supertype of `pre`.
          go(tp.join)
        case tp: JavaArrayType =>
          defn.ObjectType.findMember(name, pre, excluded)
        case err: ErrorType =>
          ctx.newErrorSymbol(pre.classSymbol orElse defn.RootClass, name, err.msg)
        case _ =>
          NoDenotation
      }
      def goRec(tp: RecType) =
        if (tp.parent == null) NoDenotation
        else {
          //println(s"find member $pre . $name in $tp")

          // We have to be careful because we might open the same (wrt eq) recursive type
          // twice during findMember which risks picking the wrong prefix in the `substRecThis(rt, pre)`
          // call below. To avoid this problem we do a defensive copy of the recursive
          // type first. But if we do this always we risk being inefficient and we ran into
          // stackoverflows when compiling pos/hk.scala under the refinement encoding
          // of hk-types. So we only do a copy if the type
          // is visited again in a recursive call to `findMember`, as tracked by `tp.opened`.
          // Furthermore, if this happens we mark the original recursive type with `openedTwice`
          // which means that we always defensively copy the type in the future. This second
          // measure is necessary because findMember calls might be cached, so do not
          // necessarily appear in nested order.
          // Without the defensive copy, Typer.scala fails to compile at the line
          //
          //      untpd.rename(lhsCore, setterName).withType(setterType), WildcardType)
          //
          // because the subtype check
          //
          //      ThisTree[Untyped]#ThisTree[Typed] <: Tree[Typed]
          //
          // fails (in fact it thinks the underlying type of the LHS is `Tree[Untyped]`.)
          //
          // Without the `openedTwice` trick, Typer.scala fails to Ycheck
          // at phase resolveSuper.
          val rt =
            if (tp.opened) { // defensive copy
              tp.openedTwice = true
              RecType(rt => tp.parent.substRecThis(tp, rt.recThis))
            } else tp
          rt.opened = true
          try go(rt.parent).mapInfo(_.substRecThis(rt, pre))
          finally {
            if (!rt.openedTwice) rt.opened = false
          }
        }

      def goRefined(tp: RefinedType) = {
        val pdenot = go(tp.parent)
        val rinfo = tp.refinedInfo
        if (name.isTypeName) { // simplified case that runs more efficiently
          val jointInfo =
            if (rinfo.isAlias) rinfo
            else if (pdenot.info.isAlias) pdenot.info
            else if (ctx.pendingMemberSearches.contains(name)) pdenot.info safe_& rinfo
            else pdenot.info recoverable_& rinfo
          pdenot.asSingleDenotation.derivedSingleDenotation(pdenot.symbol, jointInfo)
        } else {
          pdenot & (
            new JointRefDenotation(NoSymbol, rinfo, Period.allInRun(ctx.runId)),
            pre,
            safeIntersection = ctx.pendingMemberSearches.contains(name))
        }
      }

      def goApplied(tp: AppliedType) = tp.tycon match {
        case tl: HKTypeLambda =>
          go(tl.resType).mapInfo(info =>
            tl.derivedLambdaAbstraction(tl.paramNames, tl.paramInfos, info).appliedTo(tp.args))
        case tc: TypeRef if tc.symbol.isClass =>
          go(tc)
        case _ =>
          go(tp.superType)
      }

      def goHKApply(tp: HKApply) = tp.tycon match {
        case tl: HKTypeLambda =>
          go(tl.resType).mapInfo(info =>
            tl.derivedLambdaAbstraction(tl.paramNames, tl.paramInfos, info).appliedTo(tp.args))
        case _ =>
          go(tp.superType)
      }

      def goThis(tp: ThisType) = {
        val d = go(tp.underlying)
        if (d.exists) d
        else
          // There is a special case to handle:
          //   trait Super { this: Sub => private class Inner {} println(this.Inner) }
          //   class Sub extends Super
          // When resolving Super.this.Inner, the normal logic goes to the self type and
          // looks for Inner from there. But this fails because Inner is private.
          // We fix the problem by having the following fallback case, which links up the
          // member in Super instead of Sub.
          // As an example of this in the wild, see
          // loadClassWithPrivateInnerAndSubSelf in ShowClassTests
          go(tp.cls.typeRef) orElse d
      }
      def goParam(tp: TypeParamRef) = {
        val next = tp.underlying
        ctx.typerState.constraint.entry(tp) match {
          case bounds: TypeBounds if bounds ne next =>
            ctx.typerState.ephemeral = true
            go(bounds.hi)
          case _ =>
            go(next)
        }
      }
      def goSuper(tp: SuperType) = go(tp.underlying) match {
        case d: JointRefDenotation =>
          typr.println(i"redirecting super.$name from $tp to ${d.symbol.showLocated}")
          new UniqueRefDenotation(d.symbol, tp.memberInfo(d.symbol), d.validFor)
        case d => d
      }
      def goAnd(l: Type, r: Type) = {
        go(l) & (go(r), pre, safeIntersection = ctx.pendingMemberSearches.contains(name))
      }

      { val recCount = ctx.findMemberCount + 1
        ctx.findMemberCount = recCount
        if (recCount >= Config.LogPendingFindMemberThreshold) {
          ctx.pendingMemberSearches = name :: ctx.pendingMemberSearches
          if (ctx.property(TypeOps.findMemberLimit).isDefined &&
              ctx.findMemberCount > Config.PendingFindMemberLimit)
            return NoDenotation
        }
      }

      //assert(ctx.findMemberCount < 20)
      try go(this)
      catch {
        case ex: Throwable =>
          core.println(i"findMember exception for $this member $name, pre = $pre")
          throw ex // DEBUG
      }
      finally {
        val recCount = ctx.findMemberCount
        if (recCount >= Config.LogPendingFindMemberThreshold)
          ctx.pendingMemberSearches = ctx.pendingMemberSearches.tail
        ctx.findMemberCount = recCount - 1
      }
    }

    /** The set of names of members of this type that pass the given name filter
     *  when seen as members of `pre`. More precisely, these are all
     *  of members `name` such that `keepOnly(pre, name)` is `true`.
     *  @note: OK to use a Set[Name] here because Name hashcodes are replayable,
     *         hence the Set will always give the same names in the same order.
     */
    final def memberNames(keepOnly: NameFilter, pre: Type = this)(implicit ctx: Context): Set[Name] = this match {
      case tp: ClassInfo =>
        tp.cls.memberNames(keepOnly) filter (keepOnly(pre, _))
      case tp: RefinedType =>
        val ns = tp.parent.memberNames(keepOnly, pre)
        if (keepOnly(pre, tp.refinedName)) ns + tp.refinedName else ns
      case tp: TypeProxy =>
        tp.underlying.memberNames(keepOnly, pre): @tailrec
      case tp: AndType =>
        tp.tp1.memberNames(keepOnly, pre) | tp.tp2.memberNames(keepOnly, pre)
      case tp: OrType =>
        tp.tp1.memberNames(keepOnly, pre) & tp.tp2.memberNames(keepOnly, pre)
      case _ =>
        Set()
    }

    def memberDenots(keepOnly: NameFilter, f: (Name, mutable.Buffer[SingleDenotation]) => Unit)(implicit ctx: Context): Seq[SingleDenotation] = {
      val buf = mutable.ArrayBuffer[SingleDenotation]()
      for (name <- memberNames(keepOnly)) f(name, buf)
      buf
    }

    /** The set of abstract term members of this type. */
    final def abstractTermMembers(implicit ctx: Context): Seq[SingleDenotation] = track("abstractTermMembers") {
      memberDenots(abstractTermNameFilter,
          (name, buf) => buf ++= nonPrivateMember(name).altsWith(_ is Deferred))
    }

    /** The set of abstract type members of this type. */
    final def abstractTypeMembers(implicit ctx: Context): Seq[SingleDenotation] = track("abstractTypeMembers") {
      memberDenots(abstractTypeNameFilter,
          (name, buf) => buf += nonPrivateMember(name).asSingleDenotation)
    }

    /** The set of abstract type members of this type. */
    final def nonClassTypeMembers(implicit ctx: Context): Seq[SingleDenotation] = track("nonClassTypeMembers") {
      memberDenots(nonClassTypeNameFilter,
          (name, buf) => buf += member(name).asSingleDenotation)
    }

    /** The set of type members of this type */
    final def typeMembers(implicit ctx: Context): Seq[SingleDenotation] = track("typeMembers") {
      memberDenots(typeNameFilter,
          (name, buf) => buf += member(name).asSingleDenotation)
    }

    /** The set of implicit members of this type */
    final def implicitMembers(implicit ctx: Context): List[TermRef] = track("implicitMembers") {
      memberDenots(implicitFilter,
          (name, buf) => buf ++= member(name).altsWith(_ is Implicit))
        .toList.map(d => TermRef.withSig(this, d.symbol.asTerm))
    }

    /** The set of member classes of this type */
    final def memberClasses(implicit ctx: Context): Seq[SingleDenotation] = track("implicitMembers") {
      memberDenots(typeNameFilter,
        (name, buf) => buf ++= member(name).altsWith(x => x.isClass))
    }

    final def fields(implicit ctx: Context): Seq[SingleDenotation] = track("fields") {
      memberDenots(fieldFilter,
        (name, buf) => buf ++= member(name).altsWith(x => !x.is(Method)))
    }

    /** The set of members of this type having at least one of `requiredFlags` but none of `excludedFlags` set */
    final def membersBasedOnFlags(requiredFlags: FlagSet, excludedFlags: FlagSet)(implicit ctx: Context): Seq[SingleDenotation] = track("implicitMembers") {
      memberDenots(takeAllFilter,
        (name, buf) => buf ++= memberExcluding(name, excludedFlags).altsWith(x => x.is(requiredFlags)))
    }

    /** The info of `sym`, seen as a member of this type. */
    final def memberInfo(sym: Symbol)(implicit ctx: Context): Type =
      sym.info.asSeenFrom(this, sym.owner)

    /** This type seen as if it were the type of a member of prefix type `pre`
     *  declared in class `cls`.
     */
    final def asSeenFrom(pre: Type, cls: Symbol)(implicit ctx: Context): Type = track("asSeenFrom") {
      if (!cls.membersNeedAsSeenFrom(pre)) this
      else ctx.asSeenFrom(this, pre, cls)
    }

// ----- Subtype-related --------------------------------------------

    /** Is this type a subtype of that type? */
    final def <:<(that: Type)(implicit ctx: Context): Boolean = track("<:<") {
      ctx.typeComparer.topLevelSubType(this, that)
    }

    /** Is this type a subtype of that type? */
    final def frozen_<:<(that: Type)(implicit ctx: Context): Boolean = track("frozen_<:<") {
      ctx.typeComparer.isSubTypeWhenFrozen(this, that)
    }

    /** Is this type the same as that type?
     *  This is the case iff `this <:< that` and `that <:< this`.
     */
    final def =:=(that: Type)(implicit ctx: Context): Boolean = track("=:=") {
      ctx.typeComparer.isSameType(this, that)
    }

    /** Is this type a primitive value type which can be widened to the primitive value type `that`? */
    def isValueSubType(that: Type)(implicit ctx: Context) = widen match {
      case self: TypeRef if self.symbol.isPrimitiveValueClass =>
        that.widenExpr match {
          case that: TypeRef if that.symbol.isPrimitiveValueClass =>
            defn.isValueSubClass(self.symbol, that.symbol)
          case _ =>
            false
        }
      case _ =>
        false
    }

    def relaxed_<:<(that: Type)(implicit ctx: Context) =
      (this <:< that) || (this isValueSubType that)

    /** Is this type a legal type for member `sym1` that overrides another
     *  member `sym2` of type `that`? This is the same as `<:<`, except that
     *  if `matchLoosely` evaluates to true the types `=> T` and `()T` are seen
     *  as overriding each other.
     */
    final def overrides(that: Type, matchLoosely: => Boolean)(implicit ctx: Context): Boolean = {
      def widenNullary(tp: Type) = tp match {
        case tp @ MethodType(Nil) => tp.resultType
        case _ => tp
      }
      ((this.widenExpr frozen_<:< that.widenExpr) ||
        matchLoosely && {
          val this1 = widenNullary(this)
          val that1 = widenNullary(that)
          ((this1 `ne` this) || (that1 `ne` that)) && this1.overrides(this1, matchLoosely = false)
        }
      )
    }

    /** Is this type close enough to that type so that members
     *  with the two types would override each other?
     *  This means:
     *    - Either both types are polytypes with the same number of
     *      type parameters and their result types match after renaming
     *      corresponding type parameters
     *    - Or both types are method types with =:=-equivalent(*) parameter types
     *      and matching result types after renaming corresponding parameter types
     *      if the method types are dependent.
     *    - Or both types are =:=-equivalent
     *    - Or phase.erasedTypes is false, and neither type takes
     *      term or type parameters.
     *
     *  (*) when matching with a Java method, we also regard Any and Object as equivalent
     *      parameter types.
     */
    def matches(that: Type)(implicit ctx: Context): Boolean = track("matches") {
      ctx.typeComparer.matchesType(this, that, relaxed = !ctx.phase.erasedTypes)
    }

    /** This is the same as `matches` except that it also matches => T with T and
     *  vice versa.
     */
    def matchesLoosely(that: Type)(implicit ctx: Context): Boolean =
      (this matches that) || {
        val thisResult = this.widenExpr
        val thatResult = that.widenExpr
        (this eq thisResult) != (that eq thatResult) && (thisResult matchesLoosely thatResult)
      }

    /** The basetype TypeRef of this type with given class symbol,
     *  but without including any type arguments
     */
    final def baseType(base: Symbol)(implicit ctx: Context): Type = /*ctx.traceIndented(s"$this baseType $base")*/ /*>|>*/ track("baseType") /*<|<*/ {
      base.denot match {
        case classd: ClassDenotation => classd.baseTypeOf(this)
        case _ => NoType
      }
    }

    /** Temporary replacement for baseTypeRef */
    final def baseTypeTycon(base: Symbol)(implicit ctx: Context): Type =
      baseType(base).typeConstructor

    def & (that: Type)(implicit ctx: Context): Type = track("&") {
      ctx.typeComparer.glb(this, that)
    }

    /** Safer version of `&`.
     *
     *  This version does not simplify the bounds of the intersection of
     *  two TypeBounds. The simplification done by `&` requires subtyping checks
     *  which may end up calling `&` again, in most cases this should be safe
     *  but because of F-bounded types, this can result in an infinite loop
     *  (which will be masked unless `-Yno-deep-subtypes` is enabled).
     *  pos/i536 demonstrates that the infinite loop can also invole lower bounds.wait
     */
    def safe_& (that: Type)(implicit ctx: Context): Type = (this, that) match {
      case (TypeBounds(lo1, hi1), TypeBounds(lo2, hi2)) => TypeBounds(OrType(lo1, lo2), AndType(hi1, hi2))
      case _ => this & that
    }

    /** `this & that`, but handle CyclicReferences by falling back to `safe_&`.
     */
    def recoverable_&(that: Type)(implicit ctx: Context): Type =
      try this & that
      catch {
        case ex: CyclicReference => this safe_& that
          // A test case where this happens is tests/pos/i536.scala.
          // The & causes a subtype check which calls baseTypeRef again with the same
          // superclass.
      }

    def | (that: Type)(implicit ctx: Context): Type = track("|") {
      ctx.typeComparer.lub(this, that)
    }

// ----- Unwrapping types -----------------------------------------------

    /** Map a TypeVar to either its instance if it is instantiated, or its origin,
     *  if not, until the result is no longer a TypeVar. Identity on all other types.
     */
    def stripTypeVar(implicit ctx: Context): Type = this

    /** Remove all AnnotatedTypes wrapping this type.
      */
    def stripAnnots(implicit ctx: Context): Type = this

    /** Strip PolyType prefix */
    def stripPoly(implicit ctx: Context): Type = this match {
      case tp: PolyType => tp.resType.stripPoly
      case _ => this
    }

    /** Widen from singleton type to its underlying non-singleton
     *  base type by applying one or more `underlying` dereferences,
     *  Also go from => T to T.
     *  Identity for all other types. Example:
     *
     *  class Outer { class C ; val x: C }
     *  def o: Outer
     *  <o.x.type>.widen = o.C
     */
    final def widen(implicit ctx: Context): Type = widenSingleton match {
      case tp: ExprType => tp.resultType.widen
      case tp => tp
    }

    /** Widen from singleton type to its underlying non-singleton
     *  base type by applying one or more `underlying` dereferences.
     */
    final def widenSingleton(implicit ctx: Context): Type = stripTypeVar match {
      case tp: SingletonType if !tp.isOverloaded => tp.underlying.widenSingleton
      case _ => this
    }

    /** Widen from TermRef to its underlying non-termref
     *  base type, while also skipping Expr types.
     */
    final def widenTermRefExpr(implicit ctx: Context): Type = stripTypeVar match {
      case tp: TermRef if !tp.isOverloaded => tp.underlying.widenExpr.widenTermRefExpr
      case _ => this
    }

    /** Widen from ExprType type to its result type.
     *  (Note: no stripTypeVar needed because TypeVar's can't refer to ExprTypes.)
     */
    final def widenExpr: Type = this match {
      case tp: ExprType => tp.resType
      case _ => this
    }

    /** Widen type if it is unstable (i.e. an ExprType, or TermRef to unstable symbol */
    final def widenIfUnstable(implicit ctx: Context): Type = stripTypeVar match {
      case tp: ExprType => tp.resultType.widenIfUnstable
      case tp: TermRef if !tp.symbol.isStable => tp.underlying.widenIfUnstable
      case _ => this
    }

    /** If this is a skolem, its underlying type, otherwise the type itself */
    final def widenSkolem(implicit ctx: Context): Type = this match {
      case tp: SkolemType => tp.underlying
      case _ => this
    }

    /** If this type contains embedded union types, replace them by their joins.
     *  "Embedded" means: inside intersectons or recursive types, or in prefixes of refined types.
     *  If an embedded union is found, we first try to simplify or eliminate it by
     *  re-lubbing it while allowing type parameters to be constrained further.
     *  Any remaining union types are replaced by their joins.
     *
	   *  For instance, if `A` is an unconstrained type variable, then
  	 *
  	 * 	      ArrayBuffer[Int] | ArrayBuffer[A]
  	 *
     *  is approximated by constraining `A` to be =:= to `Int` and returning `ArrayBuffer[Int]`
     *  instead of `ArrayBuffer[_ >: Int | A <: Int & A]`
     */
    def widenUnion(implicit ctx: Context): Type = this match {
      case OrType(tp1, tp2) =>
        ctx.typeComparer.lub(tp1.widenUnion, tp2.widenUnion, canConstrain = true) match {
          case union: OrType => union.join
          case res => res
        }
      case tp @ AndType(tp1, tp2) =>
        tp derived_& (tp1.widenUnion, tp2.widenUnion)
      case tp: RefinedType =>
        tp.derivedRefinedType(tp.parent.widenUnion, tp.refinedName, tp.refinedInfo)
      case tp: RecType =>
        tp.rebind(tp.parent.widenUnion)
      case _ =>
        this
    }

    private def dealias(keepAnnots: Boolean)(implicit ctx: Context): Type = this match {
      case tp: TypeRef =>
        if (tp.symbol.isClass) tp
        else tp.info match {
          case TypeAlias(tp) => tp.dealias(keepAnnots): @tailrec
          case _ => tp
        }
      case app @ AppliedType(tycon, args) =>
        val tycon1 = tycon.dealias(keepAnnots)
        if (tycon1 ne tycon) app.superType.dealias(keepAnnots): @tailrec
        else this
      case app @ HKApply(tycon, args) =>
        val tycon1 = tycon.dealias(keepAnnots)
        if (tycon1 ne tycon) app.superType.dealias(keepAnnots): @tailrec
        else this
      case tp: TypeVar =>
        val tp1 = tp.instanceOpt
        if (tp1.exists) tp1.dealias(keepAnnots): @tailrec else tp
      case tp: AnnotatedType =>
        val tp1 = tp.tpe.dealias(keepAnnots)
        if (keepAnnots) tp.derivedAnnotatedType(tp1, tp.annot) else tp1
      case tp: LazyRef =>
        tp.ref.dealias(keepAnnots): @tailrec
      case _ => this
    }

    /** Follow aliases and dereferences LazyRefs and instantiated TypeVars until type
     *  is no longer alias type, LazyRef, or instantiated type variable.
     *  Goes through annotated types and rewraps annotations on the result.
     */
    final def dealiasKeepAnnots(implicit ctx: Context): Type =
      dealias(keepAnnots = true)

    /** Follow aliases and dereferences LazyRefs, annotated types and instantiated
     *  TypeVars until type is no longer alias type, annotated type, LazyRef,
     *  or instantiated type variable.
     */
    final def dealias(implicit ctx: Context): Type =
      dealias(keepAnnots = false)

    /** Perform successive widenings and dealiasings until none can be applied anymore */
    @tailrec final def widenDealias(implicit ctx: Context): Type = {
      val res = this.widen.dealias
      if (res eq this) res else res.widenDealias
    }

    /** Widen from constant type to its underlying non-constant
     *  base type.
     */
    final def deconst(implicit ctx: Context): Type = stripTypeVar match {
      case tp: ConstantType => tp.value.tpe
      case _ => this
    }

    /** The type constructor of an applied type, otherwise the type itself */
    final def typeConstructor(implicit ctx: Context): Type = this match {
      case AppliedType(tycon, _) => tycon
      case _ => this
    }

    /** If this is a (possibly aliased, annotated, and/or parameterized) reference to
     *  a class, the class type ref, otherwise NoType.
     *  @param  refinementOK   If `true` we also skip non-parameter refinements.
     */
    def underlyingClassRef(refinementOK: Boolean)(implicit ctx: Context): Type = dealias match {
      case tp: TypeRef =>
        if (tp.symbol.isClass) tp
        else if (tp.symbol.isAliasType) tp.underlying.underlyingClassRef(refinementOK)
        else NoType
      case tp: AppliedType =>
        tp.superType.underlyingClassRef(refinementOK)
      case tp: AnnotatedType =>
        tp.underlying.underlyingClassRef(refinementOK)
      case tp: RefinedType =>
        def isParamName = tp.classSymbol.typeParams.exists(_.name == tp.refinedName)
        if (refinementOK || isParamName) tp.underlying.underlyingClassRef(refinementOK)
        else NoType
      case tp: RecType =>
        tp.underlying.underlyingClassRef(refinementOK)
      case _ =>
        NoType
    }

    /** The iterator of underlying types as long as type is a TypeProxy.
     *  Useful for diagnostics
     */
    def underlyingIterator(implicit ctx: Context): Iterator[Type] = new Iterator[Type] {
      var current = Type.this
      var hasNext = true
      def next = {
        val res = current
        hasNext = current.isInstanceOf[TypeProxy]
        if (hasNext) current = current.asInstanceOf[TypeProxy].underlying
        res
      }
    }

    /** A prefix-less refined this or a termRef to a new skolem symbol
     *  that has the given type as info.
     */
    def narrow(implicit ctx: Context): TermRef =
      TermRef(NoPrefix, ctx.newSkolem(this))

    /** Useful for diagnostics: The underlying type if this type is a type proxy,
     *  otherwise NoType
     */
    def underlyingIfProxy(implicit ctx: Context) = this match {
      case this1: TypeProxy => this1.underlying
      case _ => NoType
    }

    /** If this is a repeated type, its element type, otherwise the type itself */
    def repeatedToSingle(implicit ctx: Context): Type = this match {
      case tp @ ExprType(tp1) => tp.derivedExprType(tp1.repeatedToSingle)
      case _                  => if (isRepeatedParam) this.argTypesHi.head else this
    }

    /** If this is a FunProto or PolyProto, WildcardType, otherwise this. */
    def notApplied: Type = this

    // ----- Normalizing typerefs over refined types ----------------------------

    /** If this normalizes* to a refinement type that has a refinement for `name` (which might be followed
     *  by other refinements), and the refined info is a type alias, return the alias,
     *  otherwise return NoType. Used to reduce types of the form
     *
     *    P { ... type T = / += / -= U ... } # T
     *
     *  to just U. Does not perform the reduction if the resulting type would contain
     *  a reference to the "this" of the current refined type, except in the following situation
     *
     *  (1) The "this" reference can be avoided by following an alias. Example:
     *
     *      P { type T = String, type R = P{...}.T } # R  -->  String
     *
     *  (*) normalizes means: follow instantiated typevars and aliases.
     */
    def lookupRefined(name: Name)(implicit ctx: Context): Type = {
      @tailrec def loop(pre: Type): Type = pre.stripTypeVar match {
        case pre: RefinedType =>
          pre.refinedInfo match {
            case TypeAlias(alias) =>
              if (pre.refinedName ne name) loop(pre.parent) else alias
            case _ => loop(pre.parent)
          }
        case pre: RecType =>
          val candidate = pre.parent.lookupRefined(name)
          if (candidate.exists && !pre.isReferredToBy(candidate)) {
            //println(s"lookupRefined ${this.toString} . $name, pre: $pre ---> $candidate / ${candidate.toString}")
            candidate
          }
          else NoType
        case SkolemType(tp) =>
          loop(tp)
        case pre: WildcardType =>
          WildcardType
        case pre: TypeRef =>
          pre.info match {
            case TypeAlias(alias) => loop(alias)
            case _ => NoType
          }
        case _ =>
          NoType
      }

      loop(this)
    }

    /** The type <this . name> , reduced if possible */
    def select(name: Name)(implicit ctx: Context): Type = name match {
      case name: TermName => TermRef.all(this, name)
      case name: TypeName => TypeRef(this, name).reduceProjection
    }

    /** The type <this . name> , reduced if possible, with given denotation if unreduced */
    def select(name: Name, denot: Denotation)(implicit ctx: Context): Type = name match {
      case name: TermName => TermRef(this, name, denot)
      case name: TypeName => TypeRef(this, name, denot).reduceProjection
    }

    /** The type <this . name> with given symbol, reduced if possible */
    def select(sym: Symbol)(implicit ctx: Context): Type =
      if (sym.isTerm) TermRef(this, sym.asTerm)
      else TypeRef(this, sym.asType).reduceProjection

// ----- Access to parts --------------------------------------------

    /** The normalized prefix of this type is:
     *  For an alias type, the normalized prefix of its alias
     *  For all other named type and class infos: the prefix.
     *  Inherited by all other type proxies.
     *  `NoType` for all other types.
     */
    @tailrec final def normalizedPrefix(implicit ctx: Context): Type = this match {
      case tp: NamedType =>
        if (tp.symbol.info.isAlias) tp.info.normalizedPrefix else tp.prefix
      case tp: ClassInfo =>
        tp.prefix
      case tp: TypeProxy =>
        tp.underlying.normalizedPrefix
      case _ =>
        NoType
    }

    /** For a ClassInfo type, its parents,
     *  Inherited by all type proxies. Empty for all other types.
     *  Overwritten in ClassInfo, where parents is cached.
     */
    def parentRefs(implicit ctx: Context): List[TypeRef] = this match {
      case tp: TypeProxy => tp.underlying.parentRefs
      case _ => Nil
    }

    /** The full parent types, including all type arguments */
    def parentsWithArgs(implicit ctx: Context): List[Type] = this match {
      case tp: TypeProxy => tp.superType.parentsWithArgs
      case _ => Nil
    }

    /** The full parent types, including (in new scheme) all type arguments */
    def parentsNEW(implicit ctx: Context): List[Type] = this match {
      case tp @ AppliedType(tycon, args) if tycon.typeSymbol.isClass =>
        tycon.parentsNEW.map(_.subst(tycon.typeSymbol.typeParams, args))
      case tp: TypeRef =>
        if (tp.info.isInstanceOf[TempClassInfo]) {
          tp.reloadDenot()
          assert(!tp.info.isInstanceOf[TempClassInfo])
        }
        tp.info.parentsNEW
      case tp: TypeProxy =>
        tp.superType.parentsNEW
      case _ => Nil
    }

    /** The first parent of this type, AnyRef if list of parents is empty */
    def firstParentRef(implicit ctx: Context): TypeRef = parentRefs match {
      case p :: _ => p
      case _ => defn.AnyType
    }

    /** The first parent of this type, AnyRef if list of parents is empty */
    def firstParentNEW(implicit ctx: Context): Type = parentsNEW match {
      case p :: _ => p
      case _ => defn.AnyType
    }

    /** The parameter types of a PolyType or MethodType, Empty list for others */
    final def paramInfoss(implicit ctx: Context): List[List[Type]] = stripPoly match {
      case mt: MethodType => mt.paramInfos :: mt.resultType.paramInfoss
      case _ => Nil
    }

    /** The parameter names of a PolyType or MethodType, Empty list for others */
    final def paramNamess(implicit ctx: Context): List[List[TermName]] = stripPoly match {
      case mt: MethodType => mt.paramNames :: mt.resultType.paramNamess
      case _ => Nil
    }


    /** The parameter types in the first parameter section of a generic type or MethodType, Empty list for others */
    final def firstParamTypes(implicit ctx: Context): List[Type] = stripPoly match {
      case mt: MethodType => mt.paramInfos
      case _ => Nil
    }

    /** Is this either not a method at all, or a parameterless method? */
    final def isParameterless(implicit ctx: Context): Boolean = stripPoly match {
      case mt: MethodType => false
      case _ => true
    }

    /** The resultType of a LambdaType, or ExprType, the type itself for others */
    def resultType(implicit ctx: Context): Type = this

    /** The final result type of a PolyType, MethodType, or ExprType, after skipping
     *  all parameter sections, the type itself for all others.
     */
    def finalResultType(implicit ctx: Context): Type = resultType.stripPoly match {
      case mt: MethodType => mt.resultType.finalResultType
      case _ => resultType
    }

    /** This type seen as a TypeBounds */
    final def bounds(implicit ctx: Context): TypeBounds = this match {
      case tp: TypeBounds => tp
      case ci: ClassInfo => TypeAlias(ci.appliedRef)
      case wc: WildcardType =>
        wc.optBounds match {
          case bounds: TypeBounds => bounds
          case NoType => TypeBounds.empty
        }
      case _ => TypeAlias(this)
    }

    /** The lower bound of a TypeBounds type, the type itself otherwise */
    def loBound = this match {
      case tp: TypeBounds => tp.lo
      case _ => this
    }

    /** The upper bound of a TypeBounds type, the type itself otherwise */
    def hiBound = this match {
      case tp: TypeBounds => tp.hi
      case _ => this
    }

    /** The type parameter with given `name`. This tries first `decls`
     *  in order not to provoke a cycle by forcing the info. If that yields
     *  no symbol it tries `member` as an alternative.
     */
    def typeParamNamed(name: TypeName)(implicit ctx: Context): Symbol =
      classSymbol.unforcedDecls.lookup(name) orElse member(name).symbol

    /** If this is a prototype with some ignored component, reveal one more
     *  layer of it. Otherwise the type itself.
     */
    def deepenProto(implicit ctx: Context): Type = this

// ----- Substitutions -----------------------------------------------------

    /** Substitute all types that refer in their symbol attribute to
     *  one of the symbols in `from` by the corresponding types in `to`.
     */
    final def subst(from: List[Symbol], to: List[Type])(implicit ctx: Context): Type =
      if (from.isEmpty) this
      else {
        val from1 = from.tail
        if (from1.isEmpty) ctx.subst1(this, from.head, to.head, null)
        else {
          val from2 = from1.tail
          if (from2.isEmpty) ctx.subst2(this, from.head, to.head, from1.head, to.tail.head, null)
          else ctx.subst(this, from, to, null)
        }
      }

    /** Same as `subst` but follows aliases as a fallback. When faced with a reference
     *  to an alias type, where normal substitution does not yield a new type, the
     *  substitution is instead applied to the alias. If that yields a new type,
     *  this type is returned, otherwise the original type (not the alias) is returned.
     *  A use case for this method is if one wants to substitute the type parameters
     *  of a class and also wants to substitute any parameter accessors that alias
     *  the type parameters.
     */
    final def substDealias(from: List[Symbol], to: List[Type])(implicit ctx: Context): Type =
      ctx.substDealias(this, from, to, null)

    /** Substitute all types of the form `TypeParamRef(from, N)` by
     *  `TypeParamRef(to, N)`.
     */
    final def subst(from: BindingType, to: BindingType)(implicit ctx: Context): Type =
      ctx.subst(this, from, to, null)

    /** Substitute all occurrences of `This(cls)` by `tp` */
    final def substThis(cls: ClassSymbol, tp: Type)(implicit ctx: Context): Type =
      ctx.substThis(this, cls, tp, null)

    /** As substThis, but only is class is a static owner (i.e. a globally accessible object) */
    final def substThisUnlessStatic(cls: ClassSymbol, tp: Type)(implicit ctx: Context): Type =
      if (cls.isStaticOwner) this else ctx.substThis(this, cls, tp, null)

    /** Substitute all occurrences of `RecThis(binder)` by `tp` */
    final def substRecThis(binder: RecType, tp: Type)(implicit ctx: Context): Type =
      ctx.substRecThis(this, binder, tp, null)

    /** Substitute a bound type by some other type */
    final def substParam(from: ParamRef, to: Type)(implicit ctx: Context): Type =
      ctx.substParam(this, from, to, null)

    /** Substitute bound types by some other types */
    final def substParams(from: BindingType, to: List[Type])(implicit ctx: Context): Type =
      ctx.substParams(this, from, to, null)

    /** Substitute all occurrences of symbols in `from` by references to corresponding symbols in `to`
     */
    final def substSym(from: List[Symbol], to: List[Symbol])(implicit ctx: Context): Type =
      ctx.substSym(this, from, to, null)

// ----- misc -----------------------------------------------------------

    /** Turn type into a function type.
     *  @pre this is a non-dependent method type.
     *  @param dropLast  The number of trailing parameters that should be dropped
     *                   when forming the function type.
     */
    def toFunctionType(dropLast: Int = 0)(implicit ctx: Context): Type = this match {
      case mt: MethodType if !mt.isDependent || ctx.mode.is(Mode.AllowDependentFunctions) =>
        val formals1 = if (dropLast == 0) mt.paramInfos else mt.paramInfos dropRight dropLast
        defn.FunctionOf(
          formals1 mapConserve (_.underlyingIfRepeated(mt.isJava)), mt.resultType, mt.isImplicit && !ctx.erasedTypes)
    }

    /** The signature of this type. This is by default NotAMethod,
     *  but is overridden for PolyTypes, MethodTypes, and TermRefWithSignature types.
     *  (the reason why we deviate from the "final-method-with-pattern-match-in-base-class"
     *   pattern is that method signatures use caching, so encapsulation
     *   is improved using an OO scheme).
     */
    def signature(implicit ctx: Context): Signature = Signature.NotAMethod

    /** Convert to text */
    def toText(printer: Printer): Text = printer.toText(this)

    /** Utility method to show the underlying type of a TypeProxy chain together
     *  with the proxy type itself.
     */
    def showWithUnderlying(n: Int = 1)(implicit ctx: Context): String = this match {
      case tp: TypeProxy if n > 0 => s"$show with underlying ${tp.underlying.showWithUnderlying(n - 1)}"
      case _ => show
    }

    /** A simplified version of this type which is equivalent wrt =:= to this type.
     *  This applies a typemap to the type which (as all typemaps) follows type
     *  variable instances and reduces typerefs over refined types. It also
     *  re-evaluates all occurrences of And/OrType with &/| because
     *  what was a union or intersection of type variables might be a simpler type
     *  after the type variables are instantiated. Finally, it
     *  maps poly params in the current constraint set back to their type vars.
     */
    def simplified(implicit ctx: Context) = ctx.simplify(this, null)

    /** customized hash code of this type.
     *  NotCached for uncached types. Cached types
     *  compute hash and use it as the type's hashCode.
     */
    def hash: Int
  } // end Type

// ----- Type categories ----------------------------------------------

  /** A marker trait for cached types */
  trait CachedType extends Type

  /** A marker trait for type proxies.
   *  Each implementation is expected to redefine the `underlying` method.
   */
  abstract class TypeProxy extends Type {

    /** The type to which this proxy forwards operations. */
    def underlying(implicit ctx: Context): Type

    /** The closest supertype of this type. This is the same as `underlying`,
     *  except that
     *    - instead of a TyperBounds type it returns its upper bound, and
     *    - for applied types it returns the upper bound of the constructor re-applied to the arguments.
     */
    def superType(implicit ctx: Context): Type = underlying match {
      case TypeBounds(_, hi) => hi
      case st => st
    }
  }

  // Every type has to inherit one of the following four abstract type classes.,
  // which determine whether the type is cached, and whether
  // it is a proxy of some other type. The duplication in their methods
  // is for efficiency.

  /**  Instances of this class are cached and are not proxies. */
  abstract class CachedGroundType extends Type with CachedType {
    private[this] var myHash = HashUnknown
    final def hash = {
      if (myHash == HashUnknown) {
        myHash = computeHash
        assert(myHash != HashUnknown)
      }
      myHash
    }
    override final def hashCode =
      if (hash == NotCached) System.identityHashCode(this) else hash
    def computeHash: Int
  }

  /**  Instances of this class are cached and are proxies. */
  abstract class CachedProxyType extends TypeProxy with CachedType {
    protected[this] var myHash = HashUnknown
    final def hash = {
      if (myHash == HashUnknown) {
        myHash = computeHash
        assert(myHash != HashUnknown)
      }
      myHash
    }
    override final def hashCode =
      if (hash == NotCached) System.identityHashCode(this) else hash
    def computeHash: Int
  }

  /**  Instances of this class are uncached and are not proxies. */
  abstract class UncachedGroundType extends Type {
    final def hash = NotCached
    if (monitored) {
      record(s"uncachable")
      record(s"uncachable: $getClass")
    }
  }

  /**  Instances of this class are uncached and are proxies. */
  abstract class UncachedProxyType extends TypeProxy {
    final def hash = NotCached
    if (monitored) {
      record(s"uncachable")
      record(s"uncachable: $getClass")
    }
  }

  /** A marker trait for types that apply only to type symbols */
  trait TypeType extends Type

  /** A marker trait for types that apply only to term symbols or that
   *  represent higher-kinded types.
   */
  trait TermType extends Type

  /** A marker trait for types that can be types of values or prototypes of value types */
  trait ValueTypeOrProto extends TermType

  /** A marker trait for types that can be types of values or wildcards */
  trait ValueTypeOrWildcard extends TermType

  /** A marker trait for types that can be types of values or that are higher-kinded  */
  trait ValueType extends ValueTypeOrProto with ValueTypeOrWildcard

  /** A common base trait of NamedType and AppliedType */
  trait RefType extends CachedProxyType with ValueType {
    def symbol(implicit ctx: Context): Symbol
  }

  /** A marker trait for types that are guaranteed to contain only a
   *  single non-null value (they might contain null in addition).
   */
  trait SingletonType extends TypeProxy with ValueType {
    def isOverloaded(implicit ctx: Context) = false
  }

  /** A marker trait for types that bind other types that refer to them.
   *  Instances are: LambdaType, RecType.
   */
  trait BindingType extends Type

  /** A trait for proto-types, used as expected types in typer */
  trait ProtoType extends Type {
    def isMatchedBy(tp: Type)(implicit ctx: Context): Boolean
    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context): T
    def map(tm: TypeMap)(implicit ctx: Context): ProtoType
  }

  /** Implementations of this trait cache the results of `narrow`. */
  trait NarrowCached extends Type {
    private var myNarrow: TermRef = null
    override def narrow(implicit ctx: Context): TermRef = {
      if (myNarrow eq null) myNarrow = super.narrow
      myNarrow
    }
  }

// --- NamedTypes ------------------------------------------------------------------

  /** A NamedType of the form Prefix # name */
  abstract class NamedType extends CachedProxyType with RefType {

    val prefix: Type
    val name: Name

    type ThisType >: this.type <: NamedType

    assert(prefix.isValueType || (prefix eq NoPrefix), s"invalid prefix $prefix")

    private[this] var lastDenotation: Denotation = _
    private[this] var lastSymbol: Symbol = _
    private[this] var checkedPeriod = Nowhere

    // Invariants:
    // (1) checkedPeriod != Nowhere  =>  lastDenotation != null
    // (2) lastDenotation != null    =>  lastSymbol != null

    /** There is a denotation computed which is valid (somewhere in) the
     *  current run.
     */
    def denotationIsCurrent(implicit ctx: Context) =
      lastDenotation != null && lastDenotation.validFor.runId == ctx.runId

    /** The denotation is current, its symbol, otherwise NoDenotation.
     *
     *  Note: This operation does not force the denotation, and is therefore
     *  timing dependent. It should only be used if the outcome of the
     *  essential computation does not depend on the symbol being present or not.
     *  It's currently used to take an optimized path in substituters and
     *  type accumulators, as well as to be safe in diagnostic printing.
     *  Normally, it's better to use `symbol`, not `currentSymbol`.
     */
    def currentSymbol(implicit ctx: Context) =
      if (denotationIsCurrent) symbol else NoSymbol

    /** The denotation currently denoted by this type */
    final def denot(implicit ctx: Context): Denotation = {
      val now = ctx.period
      if (checkedPeriod == now) lastDenotation else denotAt(now)
    }

    /** A first fall back to do a somewhat more expensive calculation in case the first
     *  attempt in `denot` does not yield a denotation.
     */
    private def denotAt(now: Period)(implicit ctx: Context): Denotation = {
      val d = lastDenotation
      if (d != null && (d.validFor contains now)) {
        checkedPeriod = now
        d
      }
      else computeDenot
    }

    /** Hook for adding debug check code when denotations are assigned */
    final def checkDenot()(implicit ctx: Context) = {
      if (Config.checkTypeRefCycles)
        lastDenotation match {
          case d: SingleDenotation =>
            d.infoOrCompleter match {
              case TypeBounds(lo, hi) =>
                assert(lo ne this, this)
                assert(hi ne this, this)
              case _ =>
            }
          case _ =>
        }
      if (Config.checkTypeParamRefs && Config.newScheme)
        lastDenotation match {
          case d: SingleDenotation if d.symbol.is(ClassTypeParam) =>
            prefix match {
              case prefix: Types.ThisType => assert(prefix.cls == d.symbol.owner, this)
              case _ => assert(false, this)
            }
          case _ =>
        }
    }

    /** A second fallback to recompute the denotation if necessary */
    private def computeDenot(implicit ctx: Context): Denotation = {
      val savedEphemeral = ctx.typerState.ephemeral
      ctx.typerState.ephemeral = false
      try {
        val d = lastDenotation match {
          case null =>
            val sym = lastSymbol
            if (sym == null) loadDenot else denotOfSym(sym)
          case d: SymDenotation =>
            if (this.isInstanceOf[WithFixedSym]) d.current
            else if (d.validFor.runId == ctx.runId || ctx.stillValid(d))
              if (d.exists && prefix.isTightPrefix(d.owner) || d.isConstructor) d.current
              else recomputeMember(d) // symbol could have been overridden, recompute membership
            else {
              val newd = loadDenot
              if (newd.exists) newd
              else if (ctx.mode.is(Mode.Interactive)) d
              else d.staleSymbolError
            }
          case d =>
            if (d.validFor.runId != ctx.period.runId) loadDenot
            else d.current
        }
        if (ctx.typerState.ephemeral) record("ephemeral cache miss: loadDenot")
        else if (d.exists) {
          // Avoid storing NoDenotations in the cache - we will not be able to recover from
          // them. The situation might arise that a type has NoDenotation in some later
          // phase but a defined denotation earlier (e.g. a TypeRef to an abstract type
          // is undefined after erasure.) We need to be able to do time travel back and
          // forth also in these cases.

          // Don't use setDenot here; double binding checks can give spurious failures after erasure
          lastDenotation = d
          lastSymbol = d.symbol
          checkedPeriod = ctx.period
          checkDenot()
        }
        d
      }
      finally ctx.typerState.ephemeral |= savedEphemeral
    }

    /** A member of `prefix` (disambiguated by `d.signature`) or, if none was found, `d.current`. */
    private def recomputeMember(d: SymDenotation)(implicit ctx: Context): Denotation =
      asMemberOf(prefix, allowPrivate = d.is(Private)) match {
        case NoDenotation => d.current
        case newd: SingleDenotation => newd
        case newd =>
          newd.atSignature(d.signature) match {
            case newd1: SingleDenotation if newd1.exists => newd1
            case _ => d.current
          }
      }

    private def denotOfSym(sym: Symbol)(implicit ctx: Context): Denotation = {
      val d = sym.denot
      val owner = d.owner
      if (owner.isTerm) d else d.asSeenFrom(prefix)
    }

    private def checkSymAssign(sym: Symbol)(implicit ctx: Context) = {
      def selfTypeOf(sym: Symbol) =
        if (sym.isClass) sym.asClass.givenSelfType else NoType
      assert(
        (lastSymbol eq sym)
        ||
        (lastSymbol eq null)
        || {
          val lastDefRunId = lastDenotation match {
            case d: SymDenotation => d.validFor.runId
            case _ => lastSymbol.defRunId
          }
          (lastDefRunId != sym.defRunId) ||
          (lastDefRunId == NoRunId)
        }
        ||
        lastSymbol.infoOrCompleter.isInstanceOf[ErrorType]
        ||
        sym.isPackageObject // package objects can be visited before we get around to index them
        ||
        sym.owner != lastSymbol.owner &&
          (sym.owner.derivesFrom(lastSymbol.owner)
           ||
           selfTypeOf(sym).derivesFrom(lastSymbol.owner)
           ||
           selfTypeOf(lastSymbol).derivesFrom(sym.owner)
          ),
        i"""data race? overwriting symbol of type $this,
           |long form = $toString of class $getClass,
           |last sym id = ${lastSymbol.id}, new sym id = ${sym.id},
           |last owner = ${lastSymbol.owner}, new owner = ${sym.owner},
           |period = ${ctx.phase} at run ${ctx.runId}""")
    }

    protected def sig: Signature = Signature.NotAMethod

    private[dotc] def withDenot(denot: Denotation)(implicit ctx: Context): ThisType =
      if (sig != denot.signature)
        withSig(denot.signature).withDenot(denot).asInstanceOf[ThisType]
      else {
        setDenot(denot)
        this
      }

    private[dotc] final def setDenot(denot: Denotation)(implicit ctx: Context): Unit = {
      if (Config.checkNoDoubleBindings)
        if (ctx.settings.YnoDoubleBindings.value)
          checkSymAssign(denot.symbol)

      // additional checks that intercept `denot` can be added here

      lastDenotation = denot
      checkDenot()
      lastSymbol = denot.symbol
      checkedPeriod = Nowhere
    }

    private[dotc] def withSym(sym: Symbol, signature: Signature)(implicit ctx: Context): ThisType =
      if (sig != signature)
        withSig(signature).withSym(sym, signature).asInstanceOf[ThisType]
      else {
        setSym(sym)
        this
      }

    private[dotc] final def setSym(sym: Symbol)(implicit ctx: Context): Unit = {
      if (Config.checkNoDoubleBindings)
        if (ctx.settings.YnoDoubleBindings.value)
          checkSymAssign(sym)
      uncheckedSetSym(sym)
    }

    private[dotc] final def uncheckedSetSym(sym: Symbol): Unit = {
      lastDenotation = null
      lastSymbol = sym
      checkedPeriod = Nowhere
    }

    private def withSig(sig: Signature)(implicit ctx: Context): NamedType =
      TermRef.withSig(prefix, name.asTermName, sig)

    protected def loadDenot(implicit ctx: Context): Denotation = {
      val d = asMemberOf(prefix, allowPrivate = true)
      if (d.exists || ctx.phaseId == FirstPhaseId || !lastDenotation.isInstanceOf[SymDenotation])
        d
      else { // name has changed; try load in earlier phase and make current
        val d = loadDenot(ctx.withPhase(ctx.phaseId - 1)).current
        if (d.exists) d
        else throw new Error(s"failure to reload $this of class $getClass")
      }
    }

    def reloadDenot()(implicit ctx: Context) = setDenot(loadDenot)

    protected def asMemberOf(prefix: Type, allowPrivate: Boolean)(implicit ctx: Context): Denotation =
      if (name.is(ShadowedName)) prefix.nonPrivateMember(name.exclude(ShadowedName))
      else if (!allowPrivate) prefix.nonPrivateMember(name)
      else prefix.member(name)

    /** (1) Reduce a type-ref `W # X` or `W { ... } # U`, where `W` is a wildcard type
     *  to an (unbounded) wildcard type.
     *
     *  (2) Reduce a type-ref `T { X = U; ... } # X`  to   `U`
     *  provided `U` does not refer with a RecThis to the
     *  refinement type `T { X = U; ... }`
     */
    def reduceProjection(implicit ctx: Context): Type = {
      val reduced = prefix.lookupRefined(name)
      if (reduced.exists) reduced else this
    }

    def symbol(implicit ctx: Context): Symbol = {
      val now = ctx.period
      if (checkedPeriod == now ||
          lastDenotation == null && lastSymbol != null) lastSymbol
      else denot.symbol
    }

    /** Retrieves currently valid symbol without necessarily updating denotation.
     *  Assumes that symbols do not change between periods in the same run.
     *  Used to get the class underlying a ThisType.
     */
    private[Types] def stableInRunSymbol(implicit ctx: Context): Symbol =
      if (checkedPeriod.runId == ctx.runId) lastSymbol
      else symbol

    def info(implicit ctx: Context): Type = denot.info

    def isType = isInstanceOf[TypeRef]
    def isTerm = isInstanceOf[TermRef]

    /** Guard against cycles that can arise if given `op`
     *  follows info. The problematic cases are a type alias to itself or
     *  bounded by itself or a val typed as itself:
     *
     *  type T <: T
     *  val x: x.type
     *
     *  These are errors but we have to make sure that operations do
     *  not loop before the error is detected.
     */
    final def controlled[T](op: => T)(implicit ctx: Context): T = try {
      ctx.underlyingRecursions += 1
      if (ctx.underlyingRecursions < Config.LogPendingUnderlyingThreshold)
        op
      else if (ctx.pendingUnderlying contains this)
        throw CyclicReference(symbol)
      else
        try {
          ctx.pendingUnderlying += this
          op
        } finally {
          ctx.pendingUnderlying -= this
        }
    } finally {
      ctx.underlyingRecursions -= 1
    }

    // def noArg = throw new AssertionError(s"$pre contains no matching argument for ${sym.showLocated}")

    /** The argument corresponding to class type parameter `tparam` as seen from
     *  prefix `pre`.
     */
    def argForParam(pre: Type)(implicit ctx: Context): Type = {
      val tparam = symbol
      val cls = tparam.owner
      val base = pre.baseType(cls)
      base match {
        case AppliedType(_, allArgs) =>
          var tparams = cls.typeParams
          var args = allArgs
          var idx = 0
          while (tparams.nonEmpty && args.nonEmpty) {
            if (tparams.head.eq(tparam))
              return args.head match {
                case _: TypeBounds => TypeArgRef(pre, cls.typeRef, idx)
                case arg => arg
              }
            tparams = tparams.tail
            args = args.tail
            idx += 1
          }
          NoType
        case OrType(base1, base2) => argForParam(base1) | argForParam(base2)
        case AndType(base1, base2) => argForParam(base1) & argForParam(base2)
        case _ =>
          if (pre.termSymbol is Package) argForParam(pre.select(nme.PACKAGE))
          else if (pre.isBottomType) pre
          else NoType
      }
    }

    def isClassParam(implicit ctx: Context) =
      Config.newScheme && symbol.is(TypeParam) && symbol.owner.isClass

    /** A selection of the same kind, but with potentially a different prefix.
     *  The following normalizations are performed for type selections T#A:
     *
     *     T#A --> B                if A is bound to an alias `= B` in T
     *
     *  If Config.splitProjections is set:
     *
     *     (S & T)#A --> S#A        if T does not have a member named A
     *               --> T#A        if S does not have a member named A
     *               --> S#A & T#A  otherwise
     *     (S | T)#A --> S#A | T#A
     */
    def derivedSelect(prefix: Type)(implicit ctx: Context): Type =
      if (prefix eq this.prefix) this
      else if (prefix.isBottomType) prefix
      else if (isType) {
        val res = if (isClassParam) argForParam(prefix) else prefix.lookupRefined(name)
        if (res.exists) res
        else if (Config.splitProjections)
          prefix match {
            case prefix: AndType =>
              def isMissing(tp: Type) = tp match {
                case tp: TypeRef => !tp.info.exists
                case _ => false
              }
              val derived1 = derivedSelect(prefix.tp1)
              val derived2 = derivedSelect(prefix.tp2)
              return (
                if (isMissing(derived1)) derived2
                else if (isMissing(derived2)) derived1
                else prefix.derivedAndType(derived1, derived2))
            case prefix: OrType =>
              val derived1 = derivedSelect(prefix.tp1)
              val derived2 = derivedSelect(prefix.tp2)
              return prefix.derivedOrType(derived1, derived2)
            case _ =>
              newLikeThis(prefix)
          }
        else newLikeThis(prefix)
      }
      else prefix match {
        case _: WildcardType => WildcardType
        case _ => newLikeThis(prefix)
      }

    /** Create a NamedType of the same kind as this type, but with a new prefix.
     */
    def newLikeThis(prefix: Type)(implicit ctx: Context): NamedType =
      NamedType(prefix, name)

    /** Create a NamedType of the same kind as this type, but with a "inherited name".
     *  This is necessary to in situations like the following:
     *
     *    class B { def m: T1 }
     *    class C extends B { private def m: T2; ... C.m }
     *    object C extends C
     *    object X { ... C.m }
     *
     *  The two references of C.m in class C and object X refer to different
     *  definitions: The one in C refers to C#m whereas the one in X refers to B#m.
     *  But the type C.m must have only one denotation, so it can't refer to two
     *  members depending on context.
     *
     *  In situations like this, the reference in X would get the type
     *  `<C.m>.shadowed` to make clear that we mean the inherited member, not
     *  the private one.
     *
     *  Note: An alternative, possibly more robust scheme would be to give
     *  private members special names. A private definition would have a special
     *  name (say m' in the example above), but would be entered in its enclosing
     *  under both private and public names, so it could still be found by looking up
     *  the public name.
     */
    def shadowed(implicit ctx: Context): NamedType =
      NamedType(prefix, name.derived(ShadowedName))

    override def equals(that: Any) = that match {
      case that: NamedType =>
        this.name == that.name &&
        this.prefix == that.prefix &&
        !that.isInstanceOf[TermRefWithSignature] &&
        !that.isInstanceOf[WithFixedSym]
      case _ =>
        false
    }

    /* A version of toString which also prints aliases. Can be used for debugging
    override def toString =
      if (isTerm) s"TermRef($prefix, $name)"
      else s"TypeRef($prefix, $name)${
        if (lastDenotation != null && lastDenotation.infoOrCompleter.isAlias)
          s"@@@ ${lastDenotation.infoOrCompleter.asInstanceOf[TypeAlias].hi}"
        else ""}"
    */
  }

  abstract case class TermRef(override val prefix: Type, name: TermName) extends NamedType with SingletonType {

    type ThisType = TermRef

    //assert(name.toString != "<local Coder>")
    override def underlying(implicit ctx: Context): Type = {
      val d = denot
      if (d.isOverloaded) NoType else d.info
    }

    override def signature(implicit ctx: Context): Signature = denot.signature

    override def isOverloaded(implicit ctx: Context) = denot.isOverloaded

    private def rewrap(sd: SingleDenotation)(implicit ctx: Context) =
      TermRef.withSigAndDenot(prefix, name, sd.signature, sd)

    def alternatives(implicit ctx: Context): List[TermRef] =
      denot.alternatives map rewrap

    def altsWith(p: Symbol => Boolean)(implicit ctx: Context): List[TermRef] =
      denot.altsWith(p) map rewrap
  }

  abstract case class TypeRef(override val prefix: Type, name: TypeName) extends NamedType {

    type ThisType = TypeRef

    override def underlying(implicit ctx: Context): Type = info
  }

  final class TermRefWithSignature(prefix: Type, name: TermName, override val sig: Signature) extends TermRef(prefix, name) {
    assert(prefix ne NoPrefix)
    override def signature(implicit ctx: Context) = sig
    override def loadDenot(implicit ctx: Context): Denotation = {
      val d = super.loadDenot
      if (sig eq Signature.OverloadedSignature) d
      else d.atSignature(sig).checkUnique
    }

    private def fixDenot(candidate: TermRef, prefix: Type)(implicit ctx: Context): TermRef =
      if (symbol.exists && !candidate.symbol.exists) { // recompute from previous symbol
        val ownSym = symbol
        val newd = asMemberOf(prefix, allowPrivate = ownSym.is(Private))
        candidate.withDenot(newd.suchThat(_.signature == ownSym.signature))
      }
      else candidate

    override def newLikeThis(prefix: Type)(implicit ctx: Context): TermRef = {
      // If symbol exists, the new signature is the symbol's signature as seen
      // from the new prefix, modulo consistency
      val newSig =
        if (sig == Signature.NotAMethod || !symbol.exists)
          sig
        else
          sig.updateWith(symbol.info.asSeenFrom(prefix, symbol.owner).signature)
      if (newSig ne sig)
        core.println(i"sig change at ${ctx.phase} for $this, pre = $prefix, sig: $sig --> $newSig")
      fixDenot(TermRef.withSig(prefix, name, newSig), prefix)
    }

    override def shadowed(implicit ctx: Context): NamedType =
      fixDenot(TermRef.withSig(prefix, name.derived(ShadowedName), sig), prefix)

    override def equals(that: Any) = that match {
      case that: TermRefWithSignature =>
        this.prefix == that.prefix &&
        this.name == that.name &&
        this.sig == that.sig
      case _ =>
        false
    }
    override def computeHash = doHash((name, sig), prefix)
    override def toString = super.toString ++ s"/withSig($sig)"
  }

  trait WithFixedSym extends NamedType {
    def fixedSym: Symbol
    assert(fixedSym ne NoSymbol)
    uncheckedSetSym(fixedSym)

    override def withDenot(denot: Denotation)(implicit ctx: Context): ThisType = {
      assert(denot.symbol eq fixedSym)
      setDenot(denot)
      this
    }

    override def withSym(sym: Symbol, signature: Signature)(implicit ctx: Context): ThisType =
      unsupported("withSym")

    override def newLikeThis(prefix: Type)(implicit ctx: Context): NamedType =
      NamedType.withFixedSym(prefix, fixedSym)

    override def equals(that: Any) = that match {
      case that: WithFixedSym => this.prefix == that.prefix && (this.fixedSym eq that.fixedSym)
      case _ => false
    }
    override def computeHash = doHash(fixedSym, prefix)
  }

  final class CachedTermRef(prefix: Type, name: TermName, hc: Int) extends TermRef(prefix, name) {
    assert(prefix ne NoPrefix)
    myHash = hc
    override def computeHash = unsupported("computeHash")
  }

  final class CachedTypeRef(prefix: Type, name: TypeName, hc: Int) extends TypeRef(prefix, name) {
    assert(prefix ne NoPrefix)
    myHash = hc
    override def computeHash = unsupported("computeHash")
  }

  // Those classes are non final as Linker extends them.
  class TermRefWithFixedSym(prefix: Type, name: TermName, val fixedSym: TermSymbol) extends TermRef(prefix, name) with WithFixedSym
  class TypeRefWithFixedSym(prefix: Type, name: TypeName, val fixedSym: TypeSymbol) extends TypeRef(prefix, name) with WithFixedSym

  /** Assert current phase does not have erasure semantics */
  private def assertUnerased()(implicit ctx: Context) =
    if (Config.checkUnerased) assert(!ctx.phase.erasedTypes)

  object NamedType {
    def apply(prefix: Type, name: Name)(implicit ctx: Context) =
      if (name.isTermName) TermRef.all(prefix, name.asTermName)
      else TypeRef(prefix, name.asTypeName)
    def apply(prefix: Type, name: Name, denot: Denotation)(implicit ctx: Context) =
      if (name.isTermName) TermRef(prefix, name.asTermName, denot)
      else TypeRef(prefix, name.asTypeName, denot)
    def withFixedSym(prefix: Type, sym: Symbol)(implicit ctx: Context) =
      if (sym.isType) TypeRef.withFixedSym(prefix, sym.name.asTypeName, sym.asType)
      else TermRef.withFixedSym(prefix, sym.name.asTermName, sym.asTerm)
    def withSymAndName(prefix: Type, sym: Symbol, name: Name)(implicit ctx: Context): NamedType =
      if (sym.isType) TypeRef.withSymAndName(prefix, sym.asType, name.asTypeName)
      else TermRef.withSymAndName(prefix, sym.asTerm, name.asTermName)
  }

  object TermRef {

    private def symbolicRefs(implicit ctx: Context) = ctx.phase.symbolicRefs

    /** Create term ref with given name, without specifying a signature.
     *  Its meaning is the (potentially multi-) denotation of the member(s)
     *  of prefix with given name.
     */
    def all(prefix: Type, name: TermName)(implicit ctx: Context): TermRef = {
      ctx.uniqueNamedTypes.enterIfNew(prefix, name).asInstanceOf[TermRef]
    }

    /** Create term ref referring to given symbol, taking the signature
     *  from the symbol if it is completed, or creating a term ref without
     *  signature, if symbol is not yet completed.
     */
    def apply(prefix: Type, sym: TermSymbol)(implicit ctx: Context): TermRef =
      withSymAndName(prefix, sym, sym.name)

    /** Create term ref to given initial denotation, taking the signature
     *  from the denotation if it is completed, or creating a term ref without
     *  signature, if denotation is not yet completed.
     */
    def apply(prefix: Type, name: TermName, denot: Denotation)(implicit ctx: Context): TermRef = {
      if ((prefix eq NoPrefix) || denot.symbol.isFresh || symbolicRefs)
        apply(prefix, denot.symbol.asTerm)
      else denot match {
        case denot: SymDenotation if denot.isCompleted => withSig(prefix, name, denot.signature)
        case _ => all(prefix, name)
      }
    } withDenot denot

    /** Create a non-member term ref (which cannot be reloaded using `member`),
     *  with given prefix, name, and signature
     */
    def withFixedSym(prefix: Type, name: TermName, sym: TermSymbol)(implicit ctx: Context): TermRef =
      unique(new TermRefWithFixedSym(prefix, name, sym))

    /** Create a term ref referring to given symbol with given name, taking the signature
     *  from the symbol if it is completed, or creating a term ref without
     *  signature, if symbol is not yet completed. This is very similar to TermRef(Type, Symbol),
     *  except for two differences:
     *  (1) The symbol might not yet have a denotation, so the name needs to be given explicitly.
     *  (2) The name in the term ref need not be the same as the name of the Symbol.
     */
    def withSymAndName(prefix: Type, sym: TermSymbol, name: TermName)(implicit ctx: Context): TermRef =
      if ((prefix eq NoPrefix) || sym.isFresh || symbolicRefs)
        withFixedSym(prefix, name, sym)
      else if (sym.defRunId != NoRunId && sym.isCompleted)
        withSig(prefix, name, sym.signature) withSym (sym, sym.signature)
        // Linker note:
        // this is problematic, as withSig method could return a hash-consed refference
        // that could have symbol already set making withSym trigger a double-binding error
        // ./tests/run/absoverride.scala demonstates this
      else
        all(prefix, name) withSym (sym, Signature.NotAMethod)

    /** Create a term ref to given symbol, taking the signature from the symbol
     *  (which must be completed).
     */
    def withSig(prefix: Type, sym: TermSymbol)(implicit ctx: Context): TermRef =
      if ((prefix eq NoPrefix) || sym.isFresh || symbolicRefs) withFixedSym(prefix, sym.name, sym)
      else withSig(prefix, sym.name, sym.signature).withSym(sym, sym.signature)

    /** Create a term ref with given prefix, name and signature */
    def withSig(prefix: Type, name: TermName, sig: Signature)(implicit ctx: Context): TermRef =
      unique(new TermRefWithSignature(prefix, name, sig))

    /** Create a term ref with given prefix, name, signature, and initial denotation */
    def withSigAndDenot(prefix: Type, name: TermName, sig: Signature, denot: Denotation)(implicit ctx: Context): TermRef = {
      if ((prefix eq NoPrefix) || denot.symbol.isFresh || symbolicRefs)
        withFixedSym(prefix, denot.symbol.asTerm.name, denot.symbol.asTerm)
      else
        withSig(prefix, name, sig)
    } withDenot denot
  }

  object TypeRef {
    /** Create type ref with given prefix and name */
    def apply(prefix: Type, name: TypeName)(implicit ctx: Context): TypeRef =
      ctx.uniqueNamedTypes.enterIfNew(prefix, name).asInstanceOf[TypeRef]

    /** Create type ref to given symbol */
    def apply(prefix: Type, sym: TypeSymbol)(implicit ctx: Context): TypeRef =
      withSymAndName(prefix, sym, sym.name)

    /** Create a non-member type ref  (which cannot be reloaded using `member`),
     *  with given prefix, name, and symbol.
     */
    def withFixedSym(prefix: Type, name: TypeName, sym: TypeSymbol)(implicit ctx: Context): TypeRef =
      unique(new TypeRefWithFixedSym(prefix, name, sym))

    /** Create a type ref referring to given symbol with given name.
     *  This is very similar to TypeRef(Type, Symbol),
     *  except for two differences:
     *  (1) The symbol might not yet have a denotation, so the name needs to be given explicitly.
     *  (2) The name in the type ref need not be the same as the name of the Symbol.
     */
    def withSymAndName(prefix: Type, sym: TypeSymbol, name: TypeName)(implicit ctx: Context): TypeRef =
      if ((prefix eq NoPrefix) || sym.isFresh) withFixedSym(prefix, name, sym)
      else apply(prefix, name).withSym(sym, Signature.NotAMethod)

    /** Create a type ref with given name and initial denotation */
    def apply(prefix: Type, name: TypeName, denot: Denotation)(implicit ctx: Context): TypeRef = {
      if ((prefix eq NoPrefix) || denot.symbol.isFresh) apply(prefix, denot.symbol.asType)
      else apply(prefix, name)
    } withDenot denot
  }

  // --- Other SingletonTypes: ThisType/SuperType/ConstantType ---------------------------

  /** The type cls.this
   *  @param tref    A type ref which indicates the class `cls`.
   *  Note: we do not pass a class symbol directly, because symbols
   *  do not survive runs whereas typerefs do.
   */
  abstract case class ThisType(tref: TypeRef) extends CachedProxyType with SingletonType {
    def cls(implicit ctx: Context): ClassSymbol = tref.stableInRunSymbol.asClass
    override def underlying(implicit ctx: Context): Type =
      if (ctx.erasedTypes) tref else cls.classInfo.selfType
    override def computeHash = doHash(tref)
  }

  final class CachedThisType(tref: TypeRef) extends ThisType(tref)

  object ThisType {
    /** Normally one should use ClassSymbol#thisType instead */
    def raw(tref: TypeRef)(implicit ctx: Context) =
      unique(new CachedThisType(tref))
  }

  /** The type of a super reference cls.super where
   *  `thistpe` is cls.this and `supertpe` is the type of the value referenced
   *  by `super`.
   */
  abstract case class SuperType(thistpe: Type, supertpe: Type) extends CachedProxyType with SingletonType {
    override def underlying(implicit ctx: Context) = supertpe
    override def superType(implicit ctx: Context) =
      thistpe.baseType(supertpe.typeSymbol)
    def derivedSuperType(thistpe: Type, supertpe: Type)(implicit ctx: Context) =
      if ((thistpe eq this.thistpe) && (supertpe eq this.supertpe)) this
      else SuperType(thistpe, supertpe)
    override def computeHash = doHash(thistpe, supertpe)
  }

  final class CachedSuperType(thistpe: Type, supertpe: Type) extends SuperType(thistpe, supertpe)

  object SuperType {
    def apply(thistpe: Type, supertpe: Type)(implicit ctx: Context): Type = {
      assert(thistpe != NoPrefix)
      unique(new CachedSuperType(thistpe, supertpe))
    }
  }

  /** A constant type with  single `value`. */
  abstract case class ConstantType(value: Constant) extends CachedProxyType with SingletonType {
    override def underlying(implicit ctx: Context) = value.tpe
    override def computeHash = doHash(value)
  }

  final class CachedConstantType(value: Constant) extends ConstantType(value)

  object ConstantType {
    def apply(value: Constant)(implicit ctx: Context) = {
      assertUnerased()
      unique(new CachedConstantType(value))
    }
  }

  case class LazyRef(private var refFn: Context => Type) extends UncachedProxyType with ValueType {
    private var myRef: Type = null
    private var computed = false
    def ref(implicit ctx: Context) = {
      if (computed) assert(myRef != null)
      else {
        computed = true
        myRef = refFn(ctx)
        refFn = null
      }
      myRef
    }
    def evaluating = computed && myRef == null
    override def underlying(implicit ctx: Context) = ref
    override def toString = s"LazyRef(...)"
    override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]
    override def hashCode = System.identityHashCode(this)
  }

  // --- Refined Type and RecType ------------------------------------------------

  abstract class RefinedOrRecType extends CachedProxyType with ValueType {
    def parent: Type
  }

  /** A refined type parent { refinement }
   *  @param refinedName  The name of the refinement declaration
   *  @param infoFn: A function that produces the info of the refinement declaration,
   *                 given the refined type itself.
   */
  abstract case class RefinedType(parent: Type, refinedName: Name, refinedInfo: Type) extends RefinedOrRecType {

    if (refinedName.isTermName) assert(refinedInfo.isInstanceOf[TermType])
    else assert(refinedInfo.isInstanceOf[TypeType], this)
    if (Config.newScheme) assert(!refinedName.is(NameKinds.ExpandedName), this)

    override def underlying(implicit ctx: Context) = parent

    private def badInst =
      throw new AssertionError(s"bad instantiation: $this")

    def checkInst(implicit ctx: Context): this.type = this // debug hook

    def derivedRefinedType(parent: Type, refinedName: Name, refinedInfo: Type)(implicit ctx: Context): Type =
      if ((parent eq this.parent) && (refinedName eq this.refinedName) && (refinedInfo eq this.refinedInfo)) this
      else RefinedType(parent, refinedName, refinedInfo)

    /** Add this refinement to `parent`, provided `refinedName` is a member of `parent`. */
    def wrapIfMember(parent: Type)(implicit ctx: Context): Type =
      if (parent.member(refinedName).exists) derivedRefinedType(parent, refinedName, refinedInfo)
      else parent

    override def equals(that: Any) = that match {
      case that: RefinedType =>
        this.parent == that.parent &&
        this.refinedName == that.refinedName &&
        this.refinedInfo == that.refinedInfo
      case _ =>
        false
    }
    override def computeHash = doHash(refinedName, refinedInfo, parent)
    override def toString = s"RefinedType($parent, $refinedName, $refinedInfo)"
  }

  class CachedRefinedType(parent: Type, refinedName: Name, refinedInfo: Type, hc: Int)
  extends RefinedType(parent, refinedName, refinedInfo) {
    myHash = hc
    override def computeHash = unsupported("computeHash")
  }

  object RefinedType {
    @tailrec def make(parent: Type, names: List[Name], infos: List[Type])(implicit ctx: Context): Type =
      if (names.isEmpty) parent
      else make(RefinedType(parent, names.head, infos.head), names.tail, infos.tail)

    def apply(parent: Type, name: Name, info: Type)(implicit ctx: Context): RefinedType = {
      assert(!ctx.erasedTypes)
      ctx.base.uniqueRefinedTypes.enterIfNew(parent, name, info).checkInst
    }
  }

  class RecType(parentExp: RecType => Type) extends RefinedOrRecType with BindingType {

    // See discussion in findMember#goRec why these vars are needed
    private[Types] var opened: Boolean = false
    private[Types] var openedTwice: Boolean = false

    val parent = parentExp(this)

    private[this] var myRecThis: RecThis = null

    def recThis: RecThis = {
      if (myRecThis == null) myRecThis = new RecThis(this) {}
      myRecThis
    }

    override def underlying(implicit ctx: Context): Type = parent

    def derivedRecType(parent: Type)(implicit ctx: Context): RecType =
      if (parent eq this.parent) this
      else RecType(rt => parent.substRecThis(this, rt.recThis))

    def rebind(parent: Type)(implicit ctx: Context): Type =
      if (parent eq this.parent) this
      else RecType.closeOver(rt => parent.substRecThis(this, rt.recThis))

    override def equals(other: Any) = other match {
      case other: RecType => other.parent == this.parent
      case _ => false
    }

    def isReferredToBy(tp: Type)(implicit ctx: Context): Boolean = {
      val refacc = new TypeAccumulator[Boolean] {
        override def apply(x: Boolean, tp: Type) = x || {
          tp match {
            case tp: TypeRef => apply(x, tp.prefix)
            case tp: RecThis => RecType.this eq tp.binder
            case tp: LazyRef => true // To be safe, assume a reference exists
            case _ => foldOver(x, tp)
          }
        }
      }
      refacc.apply(false, tp)
    }

    override def computeHash = doHash(parent)
    override def toString = s"RecType($parent | $hashCode)"

    private def checkInst(implicit ctx: Context): this.type = this // debug hook
  }

  object RecType {

    /** Create a RecType, normalizing its contents. This means:
     *
     *   1. Nested Rec types on the type's spine are merged with the outer one.
     *   2. Any refinement of the form `type T = z.T` on the spine of the type
     *      where `z` refers to the created rec-type is replaced by
     *      `type T`. This avoids infinite recursons later when we
     *      try to follow these references.
     *   TODO: Figure out how to guarantee absence of cycles
     *         of length > 1
     */
    def apply(parentExp: RecType => Type)(implicit ctx: Context): RecType = {
      val rt = new RecType(parentExp)
      def normalize(tp: Type): Type = tp.stripTypeVar match {
        case tp: RecType =>
          normalize(tp.parent.substRecThis(tp, rt.recThis))
        case tp @ RefinedType(parent, rname, rinfo) =>
          val rinfo1 = rinfo match {
            case TypeAlias(TypeRef(RecThis(`rt`), `rname`)) => TypeBounds.empty
            case _ => rinfo
          }
          tp.derivedRefinedType(normalize(parent), rname, rinfo1)
        case tp =>
          tp
      }
      unique(rt.derivedRecType(normalize(rt.parent))).checkInst
    }
    def closeOver(parentExp: RecType => Type)(implicit ctx: Context) = {
      val rt = this(parentExp)
      if (rt.isReferredToBy(rt.parent)) rt else rt.parent
    }
  }

  // --- AndType/OrType ---------------------------------------------------------------

  trait AndOrType extends ValueType { // todo: check where we can simplify using AndOrType
    def tp1: Type
    def tp2: Type
    def isAnd: Boolean
    def derivedAndOrType(tp1: Type, tp2: Type)(implicit ctx: Context): Type  // needed?

    private[this] var myBaseClassesPeriod: Period = Nowhere
    private[this] var myBaseClasses: List[ClassSymbol] = _

    /** Base classes of And are the merge of the operand base classes
     *  For OrTypes, it's the intersection.
     */
    override final def baseClasses(implicit ctx: Context) = {
      if (myBaseClassesPeriod != ctx.period) {
        val bcs1 = tp1.baseClasses
        val bcs1set = BaseClassSet(bcs1)
        def recur(bcs2: List[ClassSymbol]): List[ClassSymbol] = bcs2 match {
          case bc2 :: bcs2rest =>
            if (isAnd)
              if (bcs1set contains bc2)
                if (bc2.is(Trait)) recur(bcs2rest)
                else bcs1 // common class, therefore rest is the same in both sequences
              else bc2 :: recur(bcs2rest)
            else
              if (bcs1set contains bc2)
                if (bc2.is(Trait)) bc2 :: recur(bcs2rest)
                else bcs2
              else recur(bcs2rest)
          case nil =>
            if (isAnd) bcs1 else bcs2
        }
        myBaseClasses = recur(tp2.baseClasses)
        myBaseClassesPeriod = ctx.period
      }
      myBaseClasses
    }
  }

  abstract case class AndType(tp1: Type, tp2: Type) extends CachedGroundType with AndOrType {

    def isAnd = true

    def derivedAndType(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else AndType.make(tp1, tp2, checkValid = true)

    def derived_& (tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else tp1 & tp2

    def derivedAndOrType(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      derivedAndType(tp1, tp2)

    override def computeHash = doHash(tp1, tp2)
  }

  final class CachedAndType(tp1: Type, tp2: Type) extends AndType(tp1, tp2)

  object AndType {
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context): AndType = {
      assert(tp1.isInstanceOf[ValueTypeOrWildcard] &&
             tp2.isInstanceOf[ValueTypeOrWildcard], i"$tp1 & $tp2 / " + s"$tp1 & $tp2")
      unchecked(tp1, tp2)
    }

    def unchecked(tp1: Type, tp2: Type)(implicit ctx: Context): AndType = {
      assertUnerased()
      unique(new CachedAndType(tp1, tp2))
    }

    /** Make an AndType using `op` unless clearly unnecessary (i.e. without
     *  going through `&`).
     */
    def make(tp1: Type, tp2: Type, checkValid: Boolean = false)(implicit ctx: Context): Type =
      if ((tp1 eq tp2) || (tp2 eq defn.AnyType))
        tp1
      else if (tp1 eq defn.AnyType)
        tp2
      else
        if (checkValid) apply(tp1, tp2) else unchecked(tp1, tp2)
  }

  abstract case class OrType(tp1: Type, tp2: Type) extends CachedGroundType with AndOrType {

    assert(tp1.isInstanceOf[ValueTypeOrWildcard] &&
           tp2.isInstanceOf[ValueTypeOrWildcard], s"$tp1 $tp2")
    def isAnd = false

    private[this] var myJoin: Type = _
    private[this] var myJoinPeriod: Period = Nowhere

    /** Replace or type by the closest non-or type above it */
    def join(implicit ctx: Context): Type = {
      if (myJoinPeriod != ctx.period) {
        myJoin = ctx.orDominator(this)
        core.println(i"join of $this == $myJoin")
        assert(myJoin != this)
        myJoinPeriod = ctx.period
      }
      myJoin
    }

    def derivedOrType(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else OrType.make(tp1, tp2)

    def derivedAndOrType(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      derivedOrType(tp1, tp2)

    override def computeHash = doHash(tp1, tp2)
  }

  final class CachedOrType(tp1: Type, tp2: Type) extends OrType(tp1, tp2)

  object OrType {
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context) = {
      assertUnerased()
      unique(new CachedOrType(tp1, tp2))
    }
    def make(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if (tp1 eq tp2) tp1
      else apply(tp1, tp2)
  }

  // ----- ExprType and LambdaTypes -----------------------------------

  // Note: method types are cached whereas poly types are not. The reason
  // is that most poly types are cyclic via poly params,
  // and therefore two different poly types would never be equal.

  /** A trait that mixes in functionality for signature caching */
  trait MethodicType extends TermType {

    private[this] var mySignature: Signature = _
    private[this] var mySignatureRunId: Int = NoRunId

    protected def computeSignature(implicit ctx: Context): Signature

    protected def resultSignature(implicit ctx: Context) = try resultType match {
      case rtp: MethodicType => rtp.signature
      case tp =>
        if (tp.isRef(defn.UnitClass)) Signature(Nil, defn.UnitClass.fullName.asTypeName)
        else Signature(tp, isJava = false)
    }
    catch {
      case ex: AssertionError =>
        println(i"failure while taking result signature of $this: $resultType")
        throw ex
    }

    final override def signature(implicit ctx: Context): Signature = {
      if (ctx.runId != mySignatureRunId) {
        mySignature = computeSignature
        if (!mySignature.isUnderDefined) mySignatureRunId = ctx.runId
      }
      mySignature
    }
  }

  /** A by-name parameter type of the form `=> T`, or the type of a method with no parameter list. */
  abstract case class ExprType(resType: Type)
  extends CachedProxyType with TermType with MethodicType {
    override def resultType(implicit ctx: Context): Type = resType
    override def underlying(implicit ctx: Context): Type = resType
    protected def computeSignature(implicit ctx: Context): Signature = resultSignature
    def derivedExprType(resType: Type)(implicit ctx: Context) =
      if (resType eq this.resType) this else ExprType(resType)
    override def computeHash = doHash(resType)
  }

  final class CachedExprType(resultType: Type) extends ExprType(resultType)

  object ExprType {
    def apply(resultType: Type)(implicit ctx: Context) = {
      assertUnerased()
      unique(new CachedExprType(resultType))
    }
  }

  /** The lambda type square:
   *
   *    LambdaType   |   TermLambda      |   TypeLambda
   *    -------------+-------------------+------------------
   *    HKLambda     |   HKTermLambda    |   HKTypeLambda
   *    MethodOrPoly |   MethodType	     |   PolyType
   */
  trait LambdaType extends BindingType with MethodicType { self =>
    type ThisName <: Name
    type PInfo <: Type
    type This <: LambdaType{type PInfo = self.PInfo}
    type ParamRefType <: ParamRef

    def paramNames: List[ThisName]
    def paramInfos: List[PInfo]
    def resType: Type
    protected def newParamRef(n: Int): ParamRefType

    override def resultType(implicit ctx: Context) = resType

    def isJava: Boolean = false
    def isImplicit = false

    def isDependent(implicit ctx: Context): Boolean
    def isParamDependent(implicit ctx: Context): Boolean

    final def isTermLambda = isInstanceOf[TermLambda]
    final def isTypeLambda = isInstanceOf[TypeLambda]
    final def isHigherKinded = isInstanceOf[TypeProxy]

    private var myParamRefs: List[ParamRefType] = null

    def paramRefs: List[ParamRefType] = {
      if (myParamRefs == null) myParamRefs = paramNames.indices.toList.map(newParamRef)
      myParamRefs
    }

    protected def computeSignature(implicit ctx: Context) = resultSignature

    final def instantiate(argTypes: => List[Type])(implicit ctx: Context): Type =
      if (isDependent) resultType.substParams(this, argTypes)
      else resultType

    def companion: LambdaTypeCompanion[ThisName, PInfo, This]

    /** The type `[tparams := paramRefs] tp`, where `tparams` can be
     *  either a list of type parameter symbols or a list of lambda parameters
     */
    def integrate(tparams: List[ParamInfo], tp: Type)(implicit ctx: Context): Type =
      tparams match {
        case LambdaParam(lam, _) :: _ => tp.subst(lam, this)
        case tparams: List[Symbol @unchecked] => tp.subst(tparams, paramRefs)
      }

    final def derivedLambdaType(paramNames: List[ThisName] = this.paramNames,
                          paramInfos: List[PInfo] = this.paramInfos,
                          resType: Type = this.resType)(implicit ctx: Context) =
      if ((paramNames eq this.paramNames) && (paramInfos eq this.paramInfos) && (resType eq this.resType)) this
      else newLikeThis(paramNames, paramInfos, resType)

    final def newLikeThis(paramNames: List[ThisName], paramInfos: List[PInfo], resType: Type)(implicit ctx: Context): This =
      companion(paramNames)(
          x => paramInfos.mapConserve(_.subst(this, x).asInstanceOf[PInfo]),
          x => resType.subst(this, x))

    protected def prefixString: String
    final override def toString = s"$prefixString($paramNames, $paramInfos, $resType)"
  }

  abstract class HKLambda extends CachedProxyType with LambdaType {
    final override def underlying(implicit ctx: Context) = resType

    final override def computeHash = doHash(paramNames, resType, paramInfos)

    // Defined here instead of in LambdaType for efficiency
    final override def equals(that: Any) = that match {
      case that: HKLambda =>
        this.paramNames == that.paramNames &&
        this.paramInfos == that.paramInfos &&
        this.resType == that.resType &&
        (this.companion eq that.companion)
      case _ =>
        false
    }
  }

  abstract class MethodOrPoly extends CachedGroundType with LambdaType with TermType {
    final override def computeHash = doHash(paramNames, resType, paramInfos)

    // Defined here instead of in LambdaType for efficiency
    final override def equals(that: Any) = that match {
      case that: MethodOrPoly =>
        this.paramNames == that.paramNames &&
        this.paramInfos == that.paramInfos &&
        this.resType == that.resType &&
        (this.companion eq that.companion)
      case _ =>
        false
    }
  }

  trait TermLambda extends LambdaType { thisLambdaType =>
    import DepStatus._
    type ThisName = TermName
    type PInfo = Type
    type This <: TermLambda
    type ParamRefType = TermParamRef

    override def resultType(implicit ctx: Context): Type =
      if (dependencyStatus == FalseDeps) { // dealias all false dependencies
        val dealiasMap = new TypeMap {
          def apply(tp: Type) = tp match {
            case tp @ TypeRef(pre, name) =>
              tp.info match {
                case TypeAlias(alias) if depStatus(NoDeps, pre) == TrueDeps => apply(alias)
                case _ => mapOver(tp)
              }
            case _ =>
              mapOver(tp)
          }
        }
        dealiasMap(resType)
      }
      else resType

    private var myDependencyStatus: DependencyStatus = Unknown
    private var myParamDependencyStatus: DependencyStatus = Unknown

    private def depStatus(initial: DependencyStatus, tp: Type)(implicit ctx: Context): DependencyStatus = {
      def combine(x: DependencyStatus, y: DependencyStatus) = {
        val status = (x & StatusMask) max (y & StatusMask)
        val provisional = (x | y) & Provisional
        (if (status == TrueDeps) status else status | provisional).toByte
      }
      val depStatusAcc = new TypeAccumulator[DependencyStatus] {
        def apply(status: DependencyStatus, tp: Type) =
          if (status == TrueDeps) status
          else
            tp match {
              case TermParamRef(`thisLambdaType`, _) => TrueDeps
              case tp: TypeRef =>
                val status1 = foldOver(status, tp)
                tp.info match { // follow type alias to avoid dependency
                  case TypeAlias(alias) if status1 == TrueDeps && status != TrueDeps =>
                    combine(apply(status, alias), FalseDeps)
                  case _ =>
                    status1
                }
              case tp: TypeVar if !tp.isInstantiated => combine(status, Provisional)
              case _ => foldOver(status, tp)
            }
      }
      depStatusAcc(initial, tp)
    }

    /** The dependency status of this method. Some examples:
     *
     *    class C extends { type S; type T = String }
     *    def f(x: C)(y: Boolean)   // dependencyStatus = NoDeps
     *    def f(x: C)(y: x.S)       // dependencyStatus = TrueDeps
     *    def f(x: C)(y: x.T)       // dependencyStatus = FalseDeps, i.e.
     *                              // dependency can be eliminated by dealiasing.
      */
    private def dependencyStatus(implicit ctx: Context): DependencyStatus = {
      if (myDependencyStatus != Unknown) myDependencyStatus
      else {
        val result = depStatus(NoDeps, resType)
        if ((result & Provisional) == 0) myDependencyStatus = result
        (result & StatusMask).toByte
      }
    }

    /** The parameter dependency status of this method. Analogous to `dependencyStatus`,
     *  but tracking dependencies in same parameter list.
     */
    private def paramDependencyStatus(implicit ctx: Context): DependencyStatus = {
      if (myParamDependencyStatus != Unknown) myParamDependencyStatus
      else {
        val result =
          if (paramInfos.isEmpty) NoDeps
          else (NoDeps /: paramInfos.tail)(depStatus(_, _))
        if ((result & Provisional) == 0) myParamDependencyStatus = result
        (result & StatusMask).toByte
      }
    }

    /** Does result type contain references to parameters of this method type,
     *  which cannot be eliminated by de-aliasing?
     */
    def isDependent(implicit ctx: Context): Boolean = dependencyStatus == TrueDeps

    /** Does one of the parameter types contain references to earlier parameters
     *  of this method type which cannot be eliminated by de-aliasing?
     */
    def isParamDependent(implicit ctx: Context): Boolean = paramDependencyStatus == TrueDeps

    def newParamRef(n: Int) = new TermParamRef(this, n) {}
  }

  abstract case class MethodType(paramNames: List[TermName])(
      paramInfosExp: MethodType => List[Type],
      resultTypeExp: MethodType => Type)
    extends MethodOrPoly with TermLambda with NarrowCached { thisMethodType =>
    import MethodType._

    type This = MethodType

    val paramInfos = paramInfosExp(this)
    val resType = resultTypeExp(this)
    assert(resType.exists)

    override def computeSignature(implicit ctx: Context): Signature =
      resultSignature.prepend(paramInfos, isJava)

    protected def prefixString = "MethodType"
  }

  final class CachedMethodType(paramNames: List[TermName])(paramInfosExp: MethodType => List[Type], resultTypeExp: MethodType => Type)
    extends MethodType(paramNames)(paramInfosExp, resultTypeExp) {
    def companion = MethodType
  }

  final class JavaMethodType(paramNames: List[TermName])(paramInfosExp: MethodType => List[Type], resultTypeExp: MethodType => Type)
    extends MethodType(paramNames)(paramInfosExp, resultTypeExp) {
    def companion = JavaMethodType
    override def isJava = true
    override protected def prefixString = "JavaMethodType"
  }

  final class ImplicitMethodType(paramNames: List[TermName])(paramInfosExp: MethodType => List[Type], resultTypeExp: MethodType => Type)
    extends MethodType(paramNames)(paramInfosExp, resultTypeExp) {
    def companion = ImplicitMethodType
    override def isImplicit = true
    override protected def prefixString = "ImplicitMethodType"
  }

  abstract class LambdaTypeCompanion[N <: Name, PInfo <: Type, LT <: LambdaType] {
    def syntheticParamName(n: Int): N

    @sharable private val memoizedNames = new mutable.HashMap[Int, List[N]]
    def syntheticParamNames(n: Int): List[N] = synchronized {
      memoizedNames.getOrElseUpdate(n, (0 until n).map(syntheticParamName).toList)
    }

    def apply(paramNames: List[N])(paramInfosExp: LT => List[PInfo], resultTypeExp: LT => Type)(implicit ctx: Context): LT
    def apply(paramNames: List[N], paramInfos: List[PInfo], resultType: Type)(implicit ctx: Context): LT =
      apply(paramNames)(_ => paramInfos, _ => resultType)
    def apply(paramInfos: List[PInfo])(resultTypeExp: LT => Type)(implicit ctx: Context): LT =
      apply(syntheticParamNames(paramInfos.length))(_ => paramInfos, resultTypeExp)
    def apply(paramInfos: List[PInfo], resultType: Type)(implicit ctx: Context): LT =
      apply(syntheticParamNames(paramInfos.length), paramInfos, resultType)

    protected def paramName(param: ParamInfo.Of[N])(implicit ctx: Context): N =
      param.paramName

    protected def toPInfo(tp: Type)(implicit ctx: Context): PInfo

    def fromParams[PI <: ParamInfo.Of[N]](params: List[PI], resultType: Type)(implicit ctx: Context): Type =
      if (params.isEmpty) resultType
      else apply(params.map(paramName))(
        tl => params.map(param => toPInfo(tl.integrate(params, param.paramInfo))),
        tl => tl.integrate(params, resultType))
  }

  abstract class TermLambdaCompanion[LT <: TermLambda]
  extends LambdaTypeCompanion[TermName, Type, LT] {
    def toPInfo(tp: Type)(implicit ctx: Context): Type = tp
    def syntheticParamName(n: Int) = nme.syntheticParamName(n)
  }

  abstract class TypeLambdaCompanion[LT <: TypeLambda]
  extends LambdaTypeCompanion[TypeName, TypeBounds, LT] {
    def toPInfo(tp: Type)(implicit ctx: Context): TypeBounds = (tp: @unchecked) match {
      case tp: TypeBounds => tp
      case tp: ErrorType => TypeAlias(tp)
    }
    def syntheticParamName(n: Int) = tpnme.syntheticTypeParamName(n)
  }

  abstract class MethodTypeCompanion extends TermLambdaCompanion[MethodType] {

    /** Produce method type from parameter symbols, with special mappings for repeated
     *  and inline parameters:
     *   - replace @repeated annotations on Seq or Array types by <repeated> types
     *   - add @inlineParam to inline call-by-value parameters
     */
    def fromSymbols(params: List[Symbol], resultType: Type)(implicit ctx: Context) = {
      def translateRepeated(tp: Type): Type = tp match {
        case tp @ ExprType(tp1) => tp.derivedExprType(translateRepeated(tp1))
        case AnnotatedType(tp, annot) if annot matches defn.RepeatedAnnot =>
          val typeSym = tp.typeSymbol.asClass
          assert(typeSym == defn.SeqClass || typeSym == defn.ArrayClass)
          tp.translateParameterized(typeSym, defn.RepeatedParamClass)
        case tp =>
          tp
      }
      def translateInline(tp: Type): Type = tp match {
        case _: ExprType => tp
        case _ => AnnotatedType(tp, Annotation(defn.InlineParamAnnot))
      }
      def paramInfo(param: Symbol) = {
        val paramType = translateRepeated(param.info)
        if (param.is(Inline)) translateInline(paramType) else paramType
      }

      apply(params.map(_.name.asTermName))(
         tl => params.map(p => tl.integrate(params, paramInfo(p))),
         tl => tl.integrate(params, resultType))
    }

    def checkValid(mt: MethodType)(implicit ctx: Context): mt.type = {
      if (Config.checkMethodTypes)
        for ((paramInfo, idx) <- mt.paramInfos.zipWithIndex)
          paramInfo.foreachPart {
            case TermParamRef(`mt`, j) => assert(j < idx, mt)
            case _ =>
          }
      mt
    }
  }

  object MethodType extends MethodTypeCompanion {
    def apply(paramNames: List[TermName])(paramInfosExp: MethodType => List[Type], resultTypeExp: MethodType => Type)(implicit ctx: Context): MethodType =
      checkValid(unique(new CachedMethodType(paramNames)(paramInfosExp, resultTypeExp)))
  }

  object JavaMethodType extends MethodTypeCompanion {
    def apply(paramNames: List[TermName])(paramInfosExp: MethodType => List[Type], resultTypeExp: MethodType => Type)(implicit ctx: Context): MethodType =
      unique(new JavaMethodType(paramNames)(paramInfosExp, resultTypeExp))
  }

  object ImplicitMethodType extends MethodTypeCompanion {
    def apply(paramNames: List[TermName])(paramInfosExp: MethodType => List[Type], resultTypeExp: MethodType => Type)(implicit ctx: Context): MethodType =
      checkValid(unique(new ImplicitMethodType(paramNames)(paramInfosExp, resultTypeExp)))
  }

  /** A ternary extractor for MethodType */
  object MethodTpe {
    def unapply(mt: MethodType)(implicit ctx: Context) =
      Some((mt.paramNames, mt.paramInfos, mt.resultType))
  }

  trait TypeLambda extends LambdaType {
    type ThisName = TypeName
    type PInfo = TypeBounds
    type This <: TypeLambda
    type ParamRefType = TypeParamRef

    def isDependent(implicit ctx: Context): Boolean = true
    def isParamDependent(implicit ctx: Context): Boolean = true

    def newParamRef(n: Int) = new TypeParamRef(this, n) {}

    lazy val typeParams: List[LambdaParam] =
      paramNames.indices.toList.map(new LambdaParam(this, _))

    /** Instantiate parameter bounds by substituting parameters with given arguments */
    final def instantiateBounds(argTypes: List[Type])(implicit ctx: Context): List[Type] =
      paramInfos.mapConserve(_.substParams(this, argTypes))

    def derivedLambdaAbstraction(paramNames: List[TypeName], paramInfos: List[TypeBounds], resType: Type)(implicit ctx: Context): Type =
      resType match {
        case resType @ TypeAlias(alias) =>
          resType.derivedTypeAlias(newLikeThis(paramNames, paramInfos, alias))
        case resType @ TypeBounds(lo, hi) =>
          resType.derivedTypeBounds(
            if (lo.isRef(defn.NothingClass)) lo else newLikeThis(paramNames, paramInfos, lo),
            newLikeThis(paramNames, paramInfos, hi))
        case _ =>
          derivedLambdaType(paramNames, paramInfos, resType)
      }
  }

  /** A type lambda of the form `[X_0 B_0, ..., X_n B_n] => T`
   *  Variances are encoded in parameter names. A name starting with `+`
   *  designates a covariant parameter, a name starting with `-` designates
   *  a contravariant parameter, and every other name designates a non-variant parameter.
   *
   *  @param  paramNames      The names `X_0`, ..., `X_n`
   *  @param  paramInfosExp  A function that, given the polytype itself, returns the
   *                          parameter bounds `B_1`, ..., `B_n`
   *  @param  resultTypeExp   A function that, given the polytype itself, returns the
   *                          result type `T`.
   */
  class HKTypeLambda(val paramNames: List[TypeName])(
      paramInfosExp: HKTypeLambda => List[TypeBounds], resultTypeExp: HKTypeLambda => Type)
  extends HKLambda with TypeLambda {
    type This = HKTypeLambda
    def companion = HKTypeLambda

    val paramInfos: List[TypeBounds] = paramInfosExp(this)
    val resType: Type = resultTypeExp(this)

    assert(resType.isInstanceOf[TermType], this)
    assert(paramNames.nonEmpty)

    protected def prefixString = "HKTypeLambda"
  }

  /** The type of a polymorphic method. It has the same form as HKTypeLambda,
   *  except it applies to terms and parameters do not have variances.
   */
  class PolyType(val paramNames: List[TypeName])(
      paramInfosExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type)
  extends MethodOrPoly with TypeLambda {

    type This = PolyType
    def companion = PolyType

    val paramInfos: List[TypeBounds] = paramInfosExp(this)
    val resType: Type = resultTypeExp(this)

    assert(resType.isInstanceOf[TermType], this)
    assert(paramNames.nonEmpty)

    /** Merge nested polytypes into one polytype. nested polytypes are normally not supported
     *  but can arise as temporary data structures.
     */
    def flatten(implicit ctx: Context): PolyType = resType match {
      case that: PolyType =>
        val shiftedSubst = (x: PolyType) => new TypeMap {
          def apply(t: Type) = t match {
            case TypeParamRef(`that`, n) => x.paramRefs(n + paramNames.length)
            case t => mapOver(t)
          }
        }
        PolyType(paramNames ++ that.paramNames)(
          x => this.paramInfos.mapConserve(_.subst(this, x).bounds) ++
               that.paramInfos.mapConserve(shiftedSubst(x)(_).bounds),
          x => shiftedSubst(x)(that.resultType).subst(this, x))
      case _ => this
    }

    protected def prefixString = "PolyType"
  }

  object HKTypeLambda extends TypeLambdaCompanion[HKTypeLambda] {
    def apply(paramNames: List[TypeName])(
        paramInfosExp: HKTypeLambda => List[TypeBounds],
        resultTypeExp: HKTypeLambda => Type)(implicit ctx: Context): HKTypeLambda = {
      unique(new HKTypeLambda(paramNames)(paramInfosExp, resultTypeExp))
    }

    def unapply(tl: HKTypeLambda): Some[(List[LambdaParam], Type)] =
      Some((tl.typeParams, tl.resType))

    def any(n: Int)(implicit ctx: Context) =
      apply(syntheticParamNames(n))(
        pt => List.fill(n)(TypeBounds.empty), pt => defn.AnyType)

    override def paramName(param: ParamInfo.Of[TypeName])(implicit ctx: Context): TypeName =
      param.paramName.withVariance(param.paramVariance)

    /** Distributes Lambda inside type bounds. Examples:
  	 *
   	 *      type T[X] = U        becomes    type T = [X] -> U
   	 *      type T[X] <: U       becomes    type T >: Nothign <: ([X] -> U)
     *      type T[X] >: L <: U  becomes    type T >: ([X] -> L) <: ([X] -> U)
     */
    override def fromParams[PI <: ParamInfo.Of[TypeName]](params: List[PI], resultType: Type)(implicit ctx: Context): Type = {
      def expand(tp: Type) = super.fromParams(params, tp)
      resultType match {
        case rt: TypeAlias =>
          rt.derivedTypeAlias(expand(rt.alias))
        case rt @ TypeBounds(lo, hi) =>
          rt.derivedTypeBounds(
            if (lo.isRef(defn.NothingClass)) lo else expand(lo), expand(hi))
        case rt =>
          expand(rt)
      }
    }
  }

  object PolyType extends TypeLambdaCompanion[PolyType] {
    def apply(paramNames: List[TypeName])(
        paramInfosExp: PolyType => List[TypeBounds],
        resultTypeExp: PolyType => Type)(implicit ctx: Context): PolyType = {
      unique(new PolyType(paramNames)(paramInfosExp, resultTypeExp))
    }

    def unapply(tl: PolyType): Some[(List[LambdaParam], Type)] =
      Some((tl.typeParams, tl.resType))

    def any(n: Int)(implicit ctx: Context) =
      apply(syntheticParamNames(n))(
        pt => List.fill(n)(TypeBounds.empty), pt => defn.AnyType)
  }

  private object DepStatus {
    type DependencyStatus = Byte
    final val Unknown: DependencyStatus = 0   // not yet computed
    final val NoDeps: DependencyStatus = 1    // no dependent parameters found
    final val FalseDeps: DependencyStatus = 2 // all dependent parameters are prefixes of non-depended alias types
    final val TrueDeps: DependencyStatus = 3  // some truly dependent parameters exist
    final val StatusMask: DependencyStatus = 3 // the bits indicating actual dependency status
    final val Provisional: DependencyStatus = 4  // set if dependency status can still change due to type variable instantiations
  }

  // ----- HK types: LambdaParam, HKApply, TypeArgRef ---------------------

  /** The parameter of a type lambda */
  case class LambdaParam(tl: TypeLambda, n: Int) extends ParamInfo {
    type ThisName = TypeName
    def isTypeParam(implicit ctx: Context) = tl.paramNames.head.isTypeName
    def paramName(implicit ctx: Context) = tl.paramNames(n)
    def paramInfo(implicit ctx: Context) = tl.paramInfos(n)
    def paramInfoAsSeenFrom(pre: Type)(implicit ctx: Context) = paramInfo
    def paramInfoOrCompleter(implicit ctx: Context): Type = paramInfo
    def paramVariance(implicit ctx: Context): Int = tl.paramNames(n).variance
    def paramRef(implicit ctx: Context): Type = tl.paramRefs(n)
  }

  /** A type application `C[T_1, ..., T_n]` */
  abstract case class AppliedType(tycon: Type, args: List[Type])
  extends CachedProxyType with RefType {

    private var validSuper: Period = Nowhere
    private var cachedSuper: Type = _

    override def underlying(implicit ctx: Context): Type = tycon

    override def superType(implicit ctx: Context): Type = {
      if (ctx.period != validSuper) {
        validSuper = ctx.period
        cachedSuper = tycon match {
          case tycon: HKTypeLambda => defn.AnyType
          case tycon: TypeVar if !tycon.inst.exists =>
            // supertype not stable, since underlying might change
            validSuper = Nowhere
            tycon.underlying.applyIfParameterized(args)
          case tycon: TypeProxy =>
            val sym = tycon.typeSymbol
            if (sym.is(Provisional)) validSuper = Nowhere
            if (sym.isClass) tycon
            else tycon.superType.applyIfParameterized(args)
          case _ => defn.AnyType
        }
      }
      cachedSuper
    }

    override def symbol(implicit ctx: Context) = tycon.typeSymbol

    def lowerBound(implicit ctx: Context) = tycon.stripTypeVar match {
      case tycon: TypeRef =>
        tycon.info match {
          case TypeBounds(lo, hi) =>
            if (lo eq hi) superType // optimization, can profit from caching in this case
            else lo.applyIfParameterized(args)
          case _ => NoType
        }
      case _ =>
        NoType
    }

    def typeParams(implicit ctx: Context): List[ParamInfo] = {
      val tparams = tycon.typeParams
      if (tparams.isEmpty) HKTypeLambda.any(args.length).typeParams else tparams
    }

    def derivedAppliedType(tycon: Type, args: List[Type])(implicit ctx: Context): Type =
      if ((tycon eq this.tycon) && (args eq this.args)) this
      else tycon.appliedTo(args)
  }

  final class CachedAppliedType(tycon: Type, args: List[Type], hc: Int) extends AppliedType(tycon, args) {
    myHash = hc
    override def computeHash = unsupported("computeHash")
  }

  object AppliedType {
    def apply(tycon: Type, args: List[Type])(implicit ctx: Context) = {
      assertUnerased()
      ctx.base.uniqueAppliedTypes.enterIfNew(tycon, args)
    }
  }

  object ClassRef {
    def unapply(tp: RefType)(implicit ctx: Context): Option[RefType] = { // after bootstrap, drop the Option
      if (tp.symbol.isClass) Some(tp) else None
    }
  }

  /** A higher kinded type application `C[T_1, ..., T_n]` */
  abstract case class HKApply(tycon: Type, args: List[Type])
  extends CachedProxyType with ValueType {

    private var validSuper: Period = Nowhere
    private var cachedSuper: Type = _

    override def underlying(implicit ctx: Context): Type = tycon

    override def superType(implicit ctx: Context): Type = {
      if (ctx.period != validSuper) {
        validSuper = ctx.period
        cachedSuper = tycon match {
          case tp: HKTypeLambda => defn.AnyType
          case tp: TypeVar if !tp.inst.exists =>
            // supertype not stable, since underlying might change
            validSuper = Nowhere
            tp.underlying.applyIfParameterized(args)
          case tp: TypeProxy =>
            if (tp.typeSymbol.is(Provisional)) validSuper = Nowhere
            tp.superType.applyIfParameterized(args)
          case _ => defn.AnyType
        }
      }
      cachedSuper
    }

    def lowerBound(implicit ctx: Context) = tycon.stripTypeVar match {
      case tycon: TypeRef =>
        tycon.info match {
          case TypeBounds(lo, hi) =>
            if (lo eq hi) superType // optimization, can profit from caching in this case
            else lo.applyIfParameterized(args)
          case _ => NoType
        }
      case _ =>
        NoType
    }

    def typeParams(implicit ctx: Context): List[ParamInfo] = {
      val tparams = tycon.typeParams
      if (tparams.isEmpty) HKTypeLambda.any(args.length).typeParams else tparams
    }

    def derivedAppliedType(tycon: Type, args: List[Type])(implicit ctx: Context): Type =
      if ((tycon eq this.tycon) && (args eq this.args)) this
      else tycon.appliedTo(args)

    override def computeHash = doHash(tycon, args)

    protected def checkInst(implicit ctx: Context): this.type = {
      def check(tycon: Type): Unit = tycon.stripTypeVar match {
        case tycon: TypeRef if !tycon.symbol.isClass =>
        case _: TypeParamRef | _: ErrorType | _: WildcardType =>
        case _: TypeLambda =>
          assert(!args.exists(_.isInstanceOf[TypeBounds]), s"unreduced type apply: $this")
        case tycon: AnnotatedType =>
          check(tycon.underlying)
        case _ =>
          assert(false, s"illegal type constructor in $this")
      }
      if (Config.checkHKApplications) check(tycon)
      this
    }
  }

  final class CachedHKApply(tycon: Type, args: List[Type]) extends HKApply(tycon, args)

  object HKApply {
    def apply(tycon: Type, args: List[Type])(implicit ctx: Context) =
      unique(new CachedHKApply(tycon, args)).checkInst
  }

  /** A reference to wildcard argument `p.<parameter X of class C>`
   *  where `p: C[... _ ...]`
   */
  abstract case class TypeArgRef(prefix: Type, clsRef: TypeRef, idx: Int) extends CachedProxyType with ValueType {
    assert(prefix.isInstanceOf[ValueType])
    assert(idx >= 0)

    private[this] var underlyingCache: Type = _
    private[this] var underlyingCachePeriod = Nowhere

    def computeUnderlying(implicit ctx: Context): Type = {
      val cls = clsRef.symbol
      val args = prefix.baseType(cls).argInfos
      val typeParams = cls.typeParams

      val concretized = TypeArgRef.concretizeArgs(args, prefix, clsRef)
      def rebase(arg: Type) = arg.subst(typeParams, concretized)

      val arg = args(idx)
      val tparam = typeParams(idx)
      val v = tparam.paramVariance
      val pbounds = tparam.paramInfo
      if (v > 0 && pbounds.loBound.dealias.isBottomType) arg.hiBound & rebase(pbounds.hiBound)
      else if (v < 0 && pbounds.hiBound.dealias.isTopType) arg.loBound | rebase(pbounds.loBound)
      else arg recoverable_& rebase(pbounds)
    }

    override def underlying(implicit ctx: Context): Type =
      if (Config.newBoundsScheme) {
        if (!ctx.hasSameBaseTypesAs(underlyingCachePeriod)) {
          underlyingCache = computeUnderlying
          underlyingCachePeriod = ctx.period
        }
        underlyingCache
      }
      else prefix.baseType(clsRef.symbol).argInfos.apply(idx)

    def derivedTypeArgRef(prefix: Type)(implicit ctx: Context): Type =
      if (prefix eq this.prefix) this else TypeArgRef(prefix, clsRef, idx)
    override def computeHash = doHash(idx, prefix, clsRef)
  }

  final class CachedTypeArgRef(prefix: Type, clsRef: TypeRef, idx: Int) extends TypeArgRef(prefix, clsRef, idx)

  object TypeArgRef {
    def apply(prefix: Type, clsRef: TypeRef, idx: Int)(implicit ctx: Context) =
      unique(new CachedTypeArgRef(prefix, clsRef, idx))
    def fromParam(prefix: Type, tparam: TypeSymbol)(implicit ctx: Context) = {
      val cls = tparam.owner
      apply(prefix, cls.typeRef, cls.typeParams.indexOf(tparam))
    }

    def concretizeArgs(args: List[Type], prefix: Type, clsRef: TypeRef)(implicit ctx: Context): List[Type] = {
      def concretize(arg: Type, j: Int) = arg match {
        case arg: TypeBounds => TypeArgRef(prefix, clsRef, j)
        case arg => arg
      }
      args.zipWithConserve(args.indices.toList)(concretize)
    }
  }

  // ----- BoundTypes: ParamRef, RecThis ----------------------------------------

  abstract class BoundType extends CachedProxyType with ValueType {
    type BT <: Type
    val binder: BT
    def copyBoundType(bt: BT): Type
  }

  abstract class ParamRef extends BoundType {
    type BT <: LambdaType
    def paramNum: Int
    def paramName: binder.ThisName = binder.paramNames(paramNum)

    override def underlying(implicit ctx: Context): Type = {
      val infos = binder.paramInfos
      if (infos == null) NoType // this can happen if the referenced generic type is not initialized yet
      else infos(paramNum)
    }

    override def computeHash = doHash(paramNum, binder.identityHash)
    override def equals(that: Any) = that match {
      case that: ParamRef =>
        (this.binder eq that.binder) && this.paramNum == that.paramNum
      case _ =>
        false
    }

    override def toString =
      try s"ParamRef($paramName)"
      catch {
        case ex: IndexOutOfBoundsException => s"ParamRef(<bad index: $paramNum>)"
      }
  }

  /** Only created in `binder.paramRefs`. Use `binder.paramRefs(paramNum)` to
   *  refer to `TermParamRef(binder, paramNum)`.
   */
  abstract case class TermParamRef(binder: TermLambda, paramNum: Int) extends ParamRef {
    type BT = TermLambda
    def copyBoundType(bt: BT) = bt.paramRefs(paramNum)
  }

  /** Only created in `binder.paramRefs`. Use `binder.paramRefs(paramNum)` to
   *  refer to `TypeParamRef(binder, paramNum)`.
   */
  abstract case class TypeParamRef(binder: TypeLambda, paramNum: Int) extends ParamRef {
    type BT = TypeLambda
    def copyBoundType(bt: BT) = bt.paramRefs(paramNum)

    /** Looking only at the structure of `bound`, is one of the following true?
     *     - fromBelow and param <:< bound
     *     - !fromBelow and param >:> bound
     */
    def occursIn(bound: Type, fromBelow: Boolean)(implicit ctx: Context): Boolean = bound.stripTypeVar match {
      case bound: ParamRef => bound == this
      case bound: AndOrType =>
        def occ1 = occursIn(bound.tp1, fromBelow)
        def occ2 = occursIn(bound.tp2, fromBelow)
        if (fromBelow == bound.isAnd) occ1 && occ2 else occ1 || occ2
      case _ => false
    }
  }

  /** a self-reference to an enclosing recursive type. The only creation method is
   *  `binder.recThis`, returning `RecThis(binder)`.
   */
  abstract case class RecThis(binder: RecType) extends BoundType with SingletonType {
    type BT = RecType
    override def underlying(implicit ctx: Context) = binder
    def copyBoundType(bt: BT) = bt.recThis

    // need to customize hashCode and equals to prevent infinite recursion
    // between RecTypes and RecRefs.
    override def computeHash = addDelta(binder.identityHash, 41)
    override def equals(that: Any) = that match {
      case that: RecThis => this.binder eq that.binder
      case _ => false
    }
    override def toString =
      try s"RecThis(${binder.hashCode})"
      catch {
        case ex: NullPointerException => s"RecThis(<under construction>)"
      }
  }

  // ----- Skolem types -----------------------------------------------

  /** A skolem type reference with underlying type `binder`. */
  case class SkolemType(info: Type) extends UncachedProxyType with ValueType with SingletonType {
    override def underlying(implicit ctx: Context) = info
    def derivedSkolemType(info: Type)(implicit ctx: Context) =
      if (info eq this.info) this else SkolemType(info)
    override def hashCode: Int = identityHash
    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]

    def withName(name: Name): this.type = { myRepr = name; this }

    private var myRepr: Name = null
    def repr(implicit ctx: Context): Name = {
      if (myRepr == null) myRepr = SkolemName.fresh()
      myRepr
    }

    override def toString = s"Skolem($hashCode)"
  }

  // ------------ Type variables ----------------------------------------

  /** In a TypeApply tree, a TypeVar is created for each argument type to be inferred.
   *  Every type variable is referred to by exactly one inferred type parameter of some
   *  TypeApply tree.
   *
   *  A type variable is essentially a switch that models some part of a substitution.
   *  It is first linked to `origin`, a poly param that's in the current constraint set.
   *  It can then be (once) instantiated to some other type. The instantiation is
   *  recorded in the type variable itself, or else, if the current type state
   *  is different from the variable's creation state (meaning unrolls are possible)
   *  in the current typer state.
   *
   *  @param  origin        The parameter that's tracked by the type variable.
   *  @param  creatorState  The typer state in which the variable was created.
   *  @param  bindingTree   The TypeTree which introduces the type variable, or EmptyTree
   *                        if the type variable does not correspond to a source term.
   *  @paran  owner         The current owner if the context where the variable was created.
   *
   *  `owningTree` and `owner` are used to determine whether a type-variable can be instantiated
   *  at some given point. See `Inferencing#interpolateUndetVars`.
   */
  final class TypeVar(val origin: TypeParamRef, creatorState: TyperState, val bindingTree: untpd.Tree, val owner: Symbol) extends CachedProxyType with ValueType {

    /** The permanent instance type of the variable, or NoType is none is given yet */
    private[this] var myInst: Type = NoType

    private[core] def inst = myInst
    private[core] def inst_=(tp: Type) = {
      myInst = tp
      if (tp.exists) owningState = null // no longer needed; null out to avoid a memory leak
    }

    /** The state owning the variable. This is at first `creatorState`, but it can
     *  be changed to an enclosing state on a commit.
     */
    private[core] var owningState = new WeakReference(creatorState)

    /** The instance type of this variable, or NoType if the variable is currently
     *  uninstantiated
     */
    def instanceOpt(implicit ctx: Context): Type =
      if (inst.exists) inst else {
        ctx.typerState.ephemeral = true
        ctx.typerState.instType(this)
      }

    /** Is the variable already instantiated? */
    def isInstantiated(implicit ctx: Context) = instanceOpt.exists

    /** Instantiate variable with given type */
    private def instantiateWith(tp: Type)(implicit ctx: Context): Type = {
      assert(tp ne this, s"self instantiation of ${tp.show}, constraint = ${ctx.typerState.constraint.show}")
      typr.println(s"instantiating ${this.show} with ${tp.show}")
      if ((ctx.typerState eq owningState.get) && !ctx.typeComparer.subtypeCheckInProgress)
        inst = tp
      ctx.typerState.constraint = ctx.typerState.constraint.replace(origin, tp)
      tp
    }

    /** Instantiate variable from the constraints over its `origin`.
     *  If `fromBelow` is true, the variable is instantiated to the lub
     *  of its lower bounds in the current constraint; otherwise it is
     *  instantiated to the glb of its upper bounds. However, a lower bound
     *  instantiation can be a singleton type only if the upper bound
     *  is also a singleton type.
     */
    def instantiate(fromBelow: Boolean)(implicit ctx: Context): Type =
      instantiateWith(ctx.typeComparer.instanceType(origin, fromBelow))

    /** Unwrap to instance (if instantiated) or origin (if not), until result
     *  is no longer a TypeVar
     */
    override def stripTypeVar(implicit ctx: Context): Type = {
      val inst = instanceOpt
      if (inst.exists) inst.stripTypeVar else origin
    }

    /** If the variable is instantiated, its instance, otherwise its origin */
    override def underlying(implicit ctx: Context): Type = {
      val inst = instanceOpt
      if (inst.exists) inst
      else {
        ctx.typerState.ephemeral = true
        origin
      }
    }

    override def computeHash: Int = identityHash
    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]

    override def toString = {
      def instStr = if (inst.exists) s" -> $inst" else ""
      s"TypeVar($origin$instStr)"
    }
  }

  // ------ ClassInfo, Type Bounds ------------------------------------------------------------

  /** Roughly: the info of a class during a period.
   *  @param prefix       The prefix on which parents, decls, and selfType need to be rebased.
   *  @param cls          The class symbol.
   *  @param classParents The parent types of this class.
   *                      These are all normalized to be TypeRefs by moving any refinements
   *                      to be member definitions of the class itself.
   *  @param decls        The symbols defined directly in this class.
   *  @param selfInfo     The type of `this` in this class, if explicitly given,
   *                      NoType otherwise. If class is compiled from source, can also
   *                      be a reference to the self symbol containing the type.
   */
  abstract case class ClassInfo(
      prefix: Type,
      cls: ClassSymbol,
      classParentsNEW: List[Type],
      decls: Scope,
      selfInfo: DotClass /* should be: Type | Symbol */) extends CachedGroundType with TypeType {

    /** The self type of a class is the conjunction of
     *   - the explicit self type if given (or the info of a given self symbol), and
     *   - the fully applied reference to the class itself.
     */
    def selfType(implicit ctx: Context): Type = {
      if (selfTypeCache == null)
        selfTypeCache = {
          def fullRef = fullyAppliedRef
          val given = cls.givenSelfType
          val raw =
            if (!given.exists) fullRef
            else if (cls is Module) given
            else if (ctx.erasedTypes) fullRef
            else AndType(given, fullRef)
          raw//.asSeenFrom(prefix, cls.owner)
        }
      selfTypeCache
    }

    private var selfTypeCache: Type = null

    private def fullyAppliedRef(base: Type, tparams: List[TypeSymbol])(implicit ctx: Context): Type =
      if (Config.newScheme) base.appliedTo(tparams.map(_.typeRef))
      else tparams match {
        case tparam :: tparams1 =>
          fullyAppliedRef(
            RefinedType(base, tparam.name, TypeRef(cls.thisType, tparam).toBounds(tparam)),
            tparams1)
        case nil =>
          base
      }

    /** The class type with all type parameters */
    def fullyAppliedRef(implicit ctx: Context): Type =
      if (Config.newScheme && false) cls.appliedRef
      else fullyAppliedRef(cls.typeRef, cls.typeParams)

    private var appliedRefCache: Type = null
    private var typeRefCache: TypeRef = null

    def typeRef(implicit ctx: Context): TypeRef = {
      def clsDenot = if (prefix eq cls.owner.thisType) cls.denot else cls.denot.copySymDenotation(info = this)
      if (typeRefCache == null)
        typeRefCache =
          if ((cls is PackageClass) || cls.owner.isTerm) symbolicTypeRef
          else TypeRef(prefix, cls.name, clsDenot)
      typeRefCache
    }

    def appliedRef(implicit ctx: Context): Type = {
      def clsDenot = if (prefix eq cls.owner.thisType) cls.denot else cls.denot.copySymDenotation(info = this)
      if (appliedRefCache == null) {
        val tref =
          if ((cls is PackageClass) || cls.owner.isTerm) symbolicTypeRef
          else TypeRef(prefix, cls.name, clsDenot)
        appliedRefCache =
          if (Config.newScheme) tref.appliedTo(cls.typeParams.map(_.typeRef))
          else tref
      }
      appliedRefCache
    }

    def symbolicTypeRef(implicit ctx: Context): TypeRef = TypeRef(prefix, cls)

    // cached because baseType needs parents
    private var parentsCache: List[Type] = null

    /** The parent type refs as seen from the given prefix */
    override def parentRefs(implicit ctx: Context): List[TypeRef] =
      if (Config.newScheme) parentsNEW.map(_.typeConstructor.asInstanceOf[TypeRef])
      else parentsNEW.mapconserve(_.asInstanceOf[TypeRef])

    /** The parent types with all type arguments */
    override def parentsWithArgs(implicit ctx: Context): List[Type] =
      if (Config.newScheme) parentsNEW
      else parentRefs mapConserve { pref =>
        ((pref: Type) /: pref.classSymbol.typeParams) { (parent, tparam) =>
          val targSym = decls.lookup(tparam.name)
          if (targSym.exists) RefinedType(parent, targSym.name, targSym.info)
          else parent
        }
      }

    override def parentsNEW(implicit ctx: Context): List[Type] = {
      if (parentsCache == null)
        parentsCache = classParentsNEW.mapConserve(_.asSeenFrom(prefix, cls.owner))
      parentsCache
    }

    def derivedClassInfo(prefix: Type)(implicit ctx: Context) =
      if (prefix eq this.prefix) this
      else ClassInfo(prefix, cls, classParentsNEW, decls, selfInfo)

    def derivedClassInfo(prefix: Type = this.prefix, classParentsNEW: List[Type] = this.classParentsNEW, decls: Scope = this.decls, selfInfo: DotClass = this.selfInfo)(implicit ctx: Context) =
      if ((prefix eq this.prefix) && (classParentsNEW eq this.classParentsNEW) && (decls eq this.decls) && (selfInfo eq this.selfInfo)) this
      else ClassInfo(prefix, cls, classParentsNEW, decls, selfInfo)

    override def computeHash = doHash(cls, prefix)

    override def toString = s"ClassInfo($prefix, $cls, $classParentsNEW)"
  }

  class CachedClassInfo(prefix: Type, cls: ClassSymbol, classParents: List[Type], decls: Scope, selfInfo: DotClass)
    extends ClassInfo(prefix, cls, classParents, decls, selfInfo)

  /** A class for temporary class infos where `parents` are not yet known. */
  final class TempClassInfo(prefix: Type, cls: ClassSymbol, decls: Scope, selfInfo: DotClass)
  extends CachedClassInfo(prefix, cls, Nil, decls, selfInfo) {

    /** A list of actions that were because they rely on the class info of `cls` to
     *  be no longer temporary. These actions will be performed once `cls` gets a real
     *  ClassInfo.
     */
    private var suspensions: List[Context => Unit] = Nil

    def addSuspension(suspension: Context => Unit): Unit = suspensions ::= suspension

    /** Install classinfo with known parents in `denot` and resume all suspensions */
    def finalize(denot: SymDenotation, parents: List[Type])(implicit ctx: Context) = {
      denot.info = derivedClassInfo(classParentsNEW = parents)
      suspensions.foreach(_(ctx))
    }

    override def derivedClassInfo(prefix: Type)(implicit ctx: Context) =
      if (prefix eq this.prefix) this
      else new TempClassInfo(prefix, cls, decls, selfInfo)

    override def toString = s"TempClassInfo($prefix, $cls)"
  }

  object ClassInfo {
    def apply(prefix: Type, cls: ClassSymbol, classParents: List[Type], decls: Scope, selfInfo: DotClass = NoType)(implicit ctx: Context) =
      unique(new CachedClassInfo(prefix, cls, classParents, decls, selfInfo))
  }

  /** Type bounds >: lo <: hi */
  abstract case class TypeBounds(lo: Type, hi: Type) extends CachedProxyType with TypeType {

    assert(lo.isInstanceOf[TermType])
    assert(hi.isInstanceOf[TermType])

    def variance: Int = 0

    override def underlying(implicit ctx: Context): Type = hi

    /** The non-alias type bounds type with given bounds */
    def derivedTypeBounds(lo: Type, hi: Type)(implicit ctx: Context) =
      if ((lo eq this.lo) && (hi eq this.hi) && (variance == 0)) this
      else TypeBounds(lo, hi)

    /** If this is an alias, a derived alias with the new variance,
     *  Otherwise the type itself.
     */
    def withVariance(variance: Int)(implicit ctx: Context) = this match {
      case tp: TypeAlias => tp.derivedTypeAlias(tp.alias, variance)
      case _ => this
    }

    def contains(tp: Type)(implicit ctx: Context): Boolean = tp match {
      case tp: TypeBounds => lo <:< tp.lo && tp.hi <:< hi
      case tp: ClassInfo =>
        // Note: Taking a normal typeRef does not work here. A normal ref might contain
        // also other information about the named type (e.g. bounds).
        contains(tp.symbolicTypeRef)
      case _ => lo <:< tp && tp <:< hi
    }

    def & (that: TypeBounds)(implicit ctx: Context): TypeBounds =
      if ((this.lo frozen_<:< that.lo) && (that.hi frozen_<:< this.hi)) that
      else if ((that.lo frozen_<:< this.lo) && (this.hi frozen_<:< that.hi)) this
      else TypeBounds(this.lo | that.lo, this.hi & that.hi)

    def | (that: TypeBounds)(implicit ctx: Context): TypeBounds =
      if ((this.lo frozen_<:< that.lo) && (that.hi frozen_<:< this.hi)) this
      else if ((that.lo frozen_<:< this.lo) && (this.hi frozen_<:< that.hi)) that
      else TypeBounds(this.lo & that.lo, this.hi | that.hi)

    override def & (that: Type)(implicit ctx: Context) = that match {
      case that: TypeBounds => this & that
      case _ => super.& (that)
    }

    override def | (that: Type)(implicit ctx: Context) = that match {
      case that: TypeBounds => this | that
      case _ => super.| (that)
    }

    /** The implied bounds, where aliases are mapped to intervals from
     *  Nothing/Any
     */
    def boundsInterval(implicit ctx: Context): TypeBounds = this

    /** If this type and that type have the same variance, this variance, otherwise 0 */
    final def commonVariance(that: TypeBounds): Int = (this.variance + that.variance) / 2

    override def computeHash = doHash(variance, lo, hi)
    override def equals(that: Any): Boolean = that match {
      case that: TypeBounds =>
        (this.lo eq that.lo) && (this.hi eq that.hi) && (this.variance == that.variance)
      case _ =>
        false
    }

    override def toString =
      if (lo eq hi) s"TypeAlias($lo, $variance)" else s"TypeBounds($lo, $hi)"
  }

  class RealTypeBounds(lo: Type, hi: Type) extends TypeBounds(lo, hi)

  abstract class TypeAlias(val alias: Type, override val variance: Int) extends TypeBounds(alias, alias) {
    /** pre: this is a type alias */
    def derivedTypeAlias(alias: Type, variance: Int = this.variance)(implicit ctx: Context) =
      if ((alias eq this.alias) && (variance == this.variance)) this
      else TypeAlias(alias, variance)

    override def & (that: TypeBounds)(implicit ctx: Context): TypeBounds = {
      val v = this commonVariance that
      if (v > 0) derivedTypeAlias(this.hi & that.hi, v)
      else if (v < 0) derivedTypeAlias(this.lo | that.lo, v)
      else super.& (that)
    }

    override def | (that: TypeBounds)(implicit ctx: Context): TypeBounds = {
      val v = this commonVariance that
      if (v > 0) derivedTypeAlias(this.hi | that.hi, v)
      else if (v < 0) derivedTypeAlias(this.lo & that.lo, v)
      else super.| (that)
    }

    override def boundsInterval(implicit ctx: Context): TypeBounds =
      if (variance == 0) this
      else if (variance < 0) TypeBounds.lower(alias)
      else TypeBounds.upper(alias)
  }

  class CachedTypeAlias(alias: Type, variance: Int, hc: Int) extends TypeAlias(alias, variance) {
    myHash = hc
  }

  object TypeBounds {
    def apply(lo: Type, hi: Type)(implicit ctx: Context): TypeBounds =
      unique(new RealTypeBounds(lo, hi))
    def empty(implicit ctx: Context) = apply(defn.NothingType, defn.AnyType)
    def upper(hi: Type)(implicit ctx: Context) = apply(defn.NothingType, hi)
    def lower(lo: Type)(implicit ctx: Context) = apply(lo, defn.AnyType)
  }

  object TypeAlias {
    def apply(alias: Type, variance: Int = 0)(implicit ctx: Context) =
      ctx.uniqueTypeAliases.enterIfNew(alias, variance)
    def unapply(tp: TypeAlias): Option[Type] = Some(tp.alias)
  }

  // ----- Annotated and Import types -----------------------------------------------

  /** An annotated type tpe @ annot */
  case class AnnotatedType(tpe: Type, annot: Annotation)
      extends UncachedProxyType with ValueType {
    // todo: cache them? but this makes only sense if annotations and trees are also cached.
    override def underlying(implicit ctx: Context): Type = tpe
    def derivedAnnotatedType(tpe: Type, annot: Annotation) =
      if ((tpe eq this.tpe) && (annot eq this.annot)) this
      else AnnotatedType(tpe, annot)

    override def stripTypeVar(implicit ctx: Context): Type =
      derivedAnnotatedType(tpe.stripTypeVar, annot)
    override def stripAnnots(implicit ctx: Context): Type = tpe.stripAnnots
  }

  object AnnotatedType {
    def make(underlying: Type, annots: List[Annotation]) =
      (underlying /: annots)(AnnotatedType(_, _))
  }

  // Special type objects and classes -----------------------------------------------------

  /** The type of an erased array */
  abstract case class JavaArrayType(elemType: Type) extends CachedGroundType with ValueType {
    override def computeHash = doHash(elemType)
    def derivedJavaArrayType(elemtp: Type)(implicit ctx: Context) =
      if (elemtp eq this.elemType) this else JavaArrayType(elemtp)
  }
  final class CachedJavaArrayType(elemType: Type) extends JavaArrayType(elemType)
  object JavaArrayType {
    def apply(elemType: Type)(implicit ctx: Context) = unique(new CachedJavaArrayType(elemType))
  }

  /** The type of an import clause tree */
  case class ImportType(expr: Tree) extends UncachedGroundType

  /** Sentinel for "missing type" */
  @sharable case object NoType extends CachedGroundType {
    override def exists = false
    override def computeHash = hashSeed
  }

  /** Missing prefix */
  @sharable case object NoPrefix extends CachedGroundType {
    override def computeHash = hashSeed
  }

  /** A common superclass of `ErrorType` and `TryDynamicCallSite`. Instances of this
   *  class are at the same time subtypes and supertypes of every other type.
   */
  abstract class FlexType extends UncachedGroundType with ValueType

  class ErrorType private[Types] () extends FlexType {
    def msg(implicit ctx: Context): Message =
      ctx.errorTypeMsg.get(this) match {
        case Some(msgFun) => msgFun()
        case None => "error message from previous run no longer available"
      }
  }
  object ErrorType {
    def apply(msg: => Message)(implicit ctx: Context): ErrorType = {
      val et = new ErrorType
      ctx.base.errorTypeMsg(et) = () => msg
      et
    }
  }

  object UnspecifiedErrorType extends ErrorType() {
    override def msg(implicit ctx: Context): Message = "unspecified error"
  }

  /* Type used to track Select nodes that could not resolve a member and their qualifier is a scala.Dynamic. */
  object TryDynamicCallType extends FlexType

  /** Wildcard type, possibly with bounds */
  abstract case class WildcardType(optBounds: Type) extends CachedGroundType with ValueTypeOrWildcard {
    def derivedWildcardType(optBounds: Type)(implicit ctx: Context) =
      if (optBounds eq this.optBounds) this
      else if (!optBounds.exists) WildcardType
      else WildcardType(optBounds.asInstanceOf[TypeBounds])
    override def computeHash = doHash(optBounds)
  }

  final class CachedWildcardType(optBounds: Type) extends WildcardType(optBounds)

  @sharable object WildcardType extends WildcardType(NoType) {
    def apply(bounds: TypeBounds)(implicit ctx: Context) = unique(new CachedWildcardType(bounds))
  }

  /** An extractor for single abstract method types.
   *  A type is a SAM type if it is a reference to a class or trait, which
   *
   *   - has a single abstract method with a method type (ExprType
   *     and PolyType not allowed!)
   *   - can be instantiated without arguments or with just () as argument.
   *
   *  The pattern `SAMType(denot)` matches a SAM type, where `denot` is the
   *  denotation of the single abstract method as a member of the type.
   */
  object SAMType {
    def zeroParamClass(tp: Type)(implicit ctx: Context): Type = tp match {
      case tp: ClassInfo =>
        def zeroParams(tp: Type): Boolean = tp.stripPoly match {
          case mt: MethodType => mt.paramInfos.isEmpty && !mt.resultType.isInstanceOf[MethodType]
          case et: ExprType => true
          case _ => false
        }
        if ((tp.cls is Trait) || zeroParams(tp.cls.primaryConstructor.info)) tp // !!! needs to be adapted once traits have parameters
        else NoType
      case tp: AppliedType =>
        zeroParamClass(tp.superType)
      case tp: TypeRef =>
        zeroParamClass(tp.underlying)
      case tp: RefinedType =>
        zeroParamClass(tp.underlying)
      case tp: TypeBounds =>
        zeroParamClass(tp.underlying)
      case tp: TypeVar =>
        zeroParamClass(tp.underlying)
      case tp: HKApply =>
        zeroParamClass(tp.superType)
      case _ =>
        NoType
    }
    def isInstantiatable(tp: Type)(implicit ctx: Context): Boolean = zeroParamClass(tp) match {
      case cinfo: ClassInfo =>
        val tref = tp.narrow
        val selfType = cinfo.selfType.asSeenFrom(tref, cinfo.cls)
        tref <:< selfType
      case _ =>
        false
    }
    def unapply(tp: Type)(implicit ctx: Context): Option[SingleDenotation] =
      if (isInstantiatable(tp)) {
        val absMems = tp.abstractTermMembers
        // println(s"absMems: ${absMems map (_.show) mkString ", "}")
        if (absMems.size == 1)
          absMems.head.info match {
            case mt: MethodType if !mt.isDependent => Some(absMems.head)
            case _ => None
          }
        else if (tp isRef defn.PartialFunctionClass)
          // To maintain compatibility with 2.x, we treat PartialFunction specially,
          // pretending it is a SAM type. In the future it would be better to merge
          // Function and PartialFunction, have Function1 contain a isDefinedAt method
          //     def isDefinedAt(x: T) = true
          // and overwrite that method whenever the function body is a sequence of
          // case clauses.
          absMems.find(_.symbol.name == nme.apply)
        else None
      }
      else None
  }

  // ----- TypeMaps --------------------------------------------------------------------

  /** Common base class of TypeMap and TypeAccumulator */
  abstract class VariantTraversal {
    protected[core] var variance = 1

    @inline protected def atVariance[T](v: Int)(op: => T): T = {
      val saved = variance
      variance = v
      val res = op
      variance = saved
      res
    }
  }

  abstract class TypeMap(implicit protected val ctx: Context)
  extends VariantTraversal with (Type => Type) { thisMap =>

    protected def stopAtStatic = true

    def apply(tp: Type): Type

    protected def derivedSelect(tp: NamedType, pre: Type): Type =
      tp.derivedSelect(pre)  match {
        case tp: TypeArgRef if variance != 0 =>
          val tp1 = tp.underlying
          if (variance > 0) tp1.hiBound else tp1.loBound
        case tp =>
          tp
      }
    protected def derivedRefinedType(tp: RefinedType, parent: Type, info: Type): Type =
      tp.derivedRefinedType(parent, tp.refinedName, info)
    protected def derivedRecType(tp: RecType, parent: Type): Type =
      tp.rebind(parent)
    protected def derivedTypeAlias(tp: TypeAlias, alias: Type): Type =
      tp.derivedTypeAlias(alias)
    protected def derivedTypeBounds(tp: TypeBounds, lo: Type, hi: Type): Type =
      tp.derivedTypeBounds(lo, hi)
    protected def derivedSuperType(tp: SuperType, thistp: Type, supertp: Type): Type =
      tp.derivedSuperType(thistp, supertp)
    protected def derivedAppliedType(tp: AppliedType, tycon: Type, args: List[Type]): Type =
      tp.derivedAppliedType(tycon, args)
    protected def derivedAppliedType(tp: HKApply, tycon: Type, args: List[Type]): Type =
      tp.derivedAppliedType(tycon, args)
    protected def derivedTypeArgRef(tp: TypeArgRef, prefix: Type): Type =
      tp.derivedTypeArgRef(prefix)
    protected def derivedAndOrType(tp: AndOrType, tp1: Type, tp2: Type): Type =
      tp.derivedAndOrType(tp1, tp2)
    protected def derivedAnnotatedType(tp: AnnotatedType, underlying: Type, annot: Annotation): Type =
      tp.derivedAnnotatedType(underlying, annot)
    protected def derivedWildcardType(tp: WildcardType, bounds: Type): Type =
      tp.derivedWildcardType(bounds)
    protected def derivedClassInfo(tp: ClassInfo, pre: Type): Type =
      tp.derivedClassInfo(pre)
    protected def derivedJavaArrayType(tp: JavaArrayType, elemtp: Type): Type =
      tp.derivedJavaArrayType(elemtp)
    protected def derivedExprType(tp: ExprType, restpe: Type): Type =
      tp.derivedExprType(restpe)
    // note: currying needed  because Scala2 does not support param-dependencies
    protected def derivedLambdaType(tp: LambdaType)(formals: List[tp.PInfo], restpe: Type): Type =
      tp.derivedLambdaType(tp.paramNames, formals, restpe)

    /** Map this function over given type */
    def mapOver(tp: Type): Type = {
      implicit val ctx = this.ctx
      tp match {
        case tp: NamedType =>
          if (stopAtStatic && tp.symbol.isStatic) tp
          else {
            val prefix1 = atVariance(variance max 0)(this(tp.prefix))
              // A prefix is never contravariant. Even if say `p.A` is used in a contravariant
              // context, we cannot assume contravariance for `p` because `p`'s lower
              // bound might not have a binding for `A` (e.g. the lower bound could be `Nothing`).
              // By contrast, covariance does translate to the prefix, since we have that
              // if `p <: q` then `p.A <: q.A`, and well-formedness requires that `A` is a member
              // of `p`'s upper bound.
            derivedSelect(tp, prefix1)
          }
        case _: ThisType
          | _: BoundType
          | NoPrefix => tp

        case tp: AppliedType =>
          def mapArg(arg: Type, tparam: ParamInfo): Type = arg match {
            case arg: TypeBounds => this(arg)
            case _ => atVariance(variance * tparam.paramVariance)(this(arg))
          }
          derivedAppliedType(tp, this(tp.tycon),
              tp.args.zipWithConserve(tp.typeParams)(mapArg))

        case tp: RefinedType =>
          derivedRefinedType(tp, this(tp.parent), this(tp.refinedInfo))

        case tp: TypeAlias =>
          derivedTypeAlias(tp, atVariance(variance * tp.variance)(this(tp.alias)))

        case tp: TypeBounds =>
          variance = -variance
          val lo1 = this(tp.lo)
          variance = -variance
          derivedTypeBounds(tp, lo1, this(tp.hi))

        case tp: RecType =>
          derivedRecType(tp, this(tp.parent))

        case tp: TypeVar =>
          val inst = tp.instanceOpt
          if (inst.exists) apply(inst) else tp

        case tp: HKApply =>
          def mapArg(arg: Type, tparam: ParamInfo): Type =
            atVariance(variance * tparam.paramVariance)(this(arg))
          derivedAppliedType(tp, this(tp.tycon),
              tp.args.zipWithConserve(tp.typeParams)(mapArg))

        case tp: ExprType =>
          derivedExprType(tp, this(tp.resultType))

        case tp: LambdaType =>
          def mapOverLambda = {
            variance = -variance
            val ptypes1 = tp.paramInfos.mapConserve(this).asInstanceOf[List[tp.PInfo]]
            variance = -variance
            derivedLambdaType(tp)(ptypes1, this(tp.resultType))
          }
          mapOverLambda

        case tp @ TypeArgRef(prefix, _, _) =>
          derivedTypeArgRef(tp, atVariance(0)(this(prefix)))

        case tp @ SuperType(thistp, supertp) =>
          derivedSuperType(tp, this(thistp), this(supertp))

        case tp: LazyRef =>
          LazyRef(_ => this(tp.ref))

        case tp: ClassInfo =>
          mapClassInfo(tp)

         case tp: AndOrType =>
          derivedAndOrType(tp, this(tp.tp1), this(tp.tp2))

        case tp: SkolemType =>
          tp

        case tp @ AnnotatedType(underlying, annot) =>
          val underlying1 = this(underlying)
          if (underlying1 eq underlying) tp
          else derivedAnnotatedType(tp, underlying1, mapOver(annot))

        case tp: WildcardType =>
          derivedWildcardType(tp, mapOver(tp.optBounds))

        case tp: JavaArrayType =>
          derivedJavaArrayType(tp, this(tp.elemType))

        case tp: ProtoType =>
          tp.map(this)

        case _ =>
          tp
      }
    }

    private def treeTypeMap = new TreeTypeMap(typeMap = this)

    def mapOver(syms: List[Symbol]): List[Symbol] = ctx.mapSymbols(syms, treeTypeMap)

    def mapOver(scope: Scope): Scope = {
      val elems = scope.toList
      val elems1 = mapOver(elems)
      if (elems1 eq elems) scope
      else newScopeWith(elems1: _*)
    }

    def mapOver(annot: Annotation): Annotation =
      annot.derivedAnnotation(mapOver(annot.tree))

    def mapOver(tree: Tree): Tree = treeTypeMap(tree)

    /** Can be overridden. By default, only the prefix is mapped. */
    protected def mapClassInfo(tp: ClassInfo): Type =
      derivedClassInfo(tp, this(tp.prefix))

    def andThen(f: Type => Type): TypeMap = new TypeMap {
      override def stopAtStatic = thisMap.stopAtStatic
      def apply(tp: Type) = f(thisMap(tp))
    }
  }

  /** A type map that maps also parents and self type of a ClassInfo */
  abstract class DeepTypeMap(implicit ctx: Context) extends TypeMap {
    override def mapClassInfo(tp: ClassInfo) = {
      val prefix1 = this(tp.prefix)
      val parents1 = tp.parentsNEW mapConserve this
      val selfInfo1 = tp.selfInfo match {
        case selfInfo: Type => this(selfInfo)
        case selfInfo => selfInfo
      }
      tp.derivedClassInfo(prefix1, parents1, tp.decls, selfInfo1)
    }
  }

  @sharable object IdentityTypeMap extends TypeMap()(NoContext) {
    override def stopAtStatic = true
    def apply(tp: Type) = tp
  }

  /** A type map that approximates TypeBounds types depending on
   *  variance.
   *
   *  if variance > 0 : approximate by upper bound
   *     variance < 0 : approximate by lower bound
   *     variance = 0 : propagate bounds to next outer level
   */
  abstract class ApproximatingTypeMap(implicit ctx: Context) extends TypeMap { thisMap =>

    protected def range(lo: Type, hi: Type) =
      if (variance > 0) hi
      else if (variance < 0) lo
      else Range(lower(lo), upper(hi))

    protected def isRange(tp: Type) = tp.isInstanceOf[Range]

    protected def lower(tp: Type) = tp match {
      case tp: Range => tp.lo
      case _ => tp
    }

    protected def upper(tp: Type) = tp match {
      case tp: Range => tp.hi
      case _ => tp
    }

    protected def rangeToBounds(tp: Type) = tp match {
      case Range(lo, hi) => TypeBounds(lo, hi)
      case _ => tp
    }

    /** Try to widen a named type to its info relative to given prefix `pre`, where possible.
     *  The possible cases are listed inline in the code. Return `default` if no widening is
     *  possible.
     */
    def tryWiden(tp: NamedType, pre: Type): Type =
      pre.member(tp.name) match {
        case d: SingleDenotation =>
          d.info match {
            case TypeAlias(alias) =>
              // if H#T = U, then for any x in L..H, x.T =:= U,
              // hence we can replace with U under all variances
              reapply(alias)
            case TypeBounds(lo, hi) =>
              // If H#T = _ >: S <: U, then for any x in L..H, S <: x.T <: U,
              // hence we can replace with S..U under all variances
              range(atVariance(-variance)(reapply(lo)), reapply(hi))
            case info: SingletonType =>
              // if H#x: y.type, then for any x in L..H, x.type =:= y.type,
              // hence we can replace with y.type under all variances
              reapply(info)
            case _ =>
              NoType
          }
        case _ => NoType
      }

    /** Derived selection.
     *  @pre   the (upper bound of) prefix `pre` has a member named `tp.name`.
     */
    override protected def derivedSelect(tp: NamedType, pre: Type) =
      if (pre eq tp.prefix) tp
      else pre match {
        case Range(preLo, preHi) =>
          val forwarded =
            if (tp.isClassParam) tp.argForParam(preHi)
            else tryWiden(tp, preHi)
          forwarded.orElse(
            range(super.derivedSelect(tp, preLo), super.derivedSelect(tp, preHi)))
        case _ =>
          super.derivedSelect(tp, pre)
      }

    override protected def derivedRefinedType(tp: RefinedType, parent: Type, info: Type) =
      if ((parent eq tp.parent) && (info eq tp.refinedInfo)) tp
      else parent match {
        case Range(parentLo, parentHi) =>
          range(derivedRefinedType(tp, parentLo, info), derivedRefinedType(tp, parentHi, info))
        case _ =>
          def propagate(lo: Type, hi: Type) =
            range(derivedRefinedType(tp, parent, lo), derivedRefinedType(tp, parent, hi))
          if (parent.isBottomType) parent
          else info match {
            case Range(infoLo: TypeBounds, infoHi: TypeBounds) =>
              assert(variance == 0)
              val v1 = infoLo.variance
              val v2 = infoHi.variance
              // There's some weirdness coming from the way aliases can have variance
              // If infoLo and infoHi are both aliases with the same non-zero variance
              // we can propagate to a range of the refined types. If they are both
              // non-alias ranges we know that infoLo <:< infoHi and therefore we can
              // propagate to refined types with infoLo and infoHi as bounds.
              // In all other cases, Nothing..Any is the only interval that contains
              // the range. i966.scala is a test case.
              if (v1 > 0 && v2 > 0) propagate(infoLo, infoHi)
              else if (v1 < 0 && v2 < 0) propagate(infoHi, infoLo)
              else if (!infoLo.isAlias && !infoHi.isAlias) propagate(infoLo, infoHi)
              else range(tp.bottomType, tp.topType)
                // Using `parent` instead of `tp.topType` would be better for normal refinements,
                // but it would also turn *-types into hk-types, which is not what we want.
                // We should revisit this point in case we represent applied types not as refinements anymore.
            case Range(infoLo, infoHi) =>
              propagate(infoLo, infoHi)
            case _ =>
              tp.derivedRefinedType(parent, tp.refinedName, info)
          }
        }

    override protected def derivedRecType(tp: RecType, parent: Type) =
      if (parent eq tp.parent) tp
      else parent match {
        case Range(lo, hi) => range(tp.rebind(lo), tp.rebind(hi))
        case _ => tp.rebind(parent)
      }

    override protected def derivedTypeAlias(tp: TypeAlias, alias: Type) =
      if (alias eq tp.alias) tp
      else alias match {
        case Range(lo, hi) =>
          if (variance > 0) TypeBounds(lo, hi)
          else range(TypeAlias(lo), TypeAlias(hi))
        case _ => tp.derivedTypeAlias(alias)
      }

    override protected def derivedTypeBounds(tp: TypeBounds, lo: Type, hi: Type) =
      if ((lo eq tp.lo) && (hi eq tp.hi)) tp
      else if (isRange(lo) || isRange(hi))
        if (variance > 0) TypeBounds(lower(lo), upper(hi))
        else range(TypeBounds(upper(lo), lower(hi)), TypeBounds(lower(lo), upper(hi)))
      else tp.derivedTypeBounds(lo, hi)

    override protected def derivedSuperType(tp: SuperType, thistp: Type, supertp: Type) =
      if (isRange(thistp) || isRange(supertp)) range(thistp.bottomType, thistp.topType)
      else tp.derivedSuperType(thistp, supertp)

    override protected def derivedAppliedType(tp: AppliedType, tycon: Type, args: List[Type]): Type =
      tycon match {
        case Range(tyconLo, tyconHi) =>
          range(derivedAppliedType(tp, tyconLo, args), derivedAppliedType(tp, tyconHi, args))
        case _ =>
          if (args.exists(isRange)) {
            if (variance > 0) tp.derivedAppliedType(tycon, args.map(rangeToBounds))
            else {
              val loBuf, hiBuf = new mutable.ListBuffer[Type]
              // Given `C[A1, ..., An]` where sone A's are ranges, try to find
              // non-range arguments L1, ..., Ln and H1, ..., Hn such that
              // C[L1, ..., Ln] <: C[H1, ..., Hn] by taking the right limits of
              // ranges that appear in as co- or contravariant arguments.
              // Fail for non-variant argument ranges.
              // If successful, the L-arguments are in loBut, the H-arguments in hiBuf.
              // @return  operation succeeded for all arguments.
              def distributeArgs(args: List[Type], tparams: List[ParamInfo]): Boolean = args match {
                case Range(lo, hi) :: args1 =>
                  val v = tparams.head.paramVariance
                  if (v == 0) false
                  else {
                    if (v > 0) { loBuf += lo; hiBuf += hi }
                    else { loBuf += hi; hiBuf += lo }
                    distributeArgs(args1, tparams.tail)
                  }
                case arg :: args1 =>
                  loBuf += arg; hiBuf += arg
                  distributeArgs(args1, tparams.tail)
                case nil =>
                  true
              }
              if (distributeArgs(args, tp.typeParams))
                range(tp.derivedAppliedType(tycon, loBuf.toList),
                      tp.derivedAppliedType(tycon, hiBuf.toList))
              else range(tp.bottomType, tp.topType)
                // TODO: can we give a better bound than `topType`?
            }
          }
          else tp.derivedAppliedType(tycon, args)
      }

    override protected def derivedAppliedType(tp: HKApply, tycon: Type, args: List[Type]): Type =
      tycon match {
        case Range(tyconLo, tyconHi) =>
          range(derivedAppliedType(tp, tyconLo, args), derivedAppliedType(tp, tyconHi, args))
        case _ =>
          if (args.exists(isRange)) {
            if (variance > 0) tp.derivedAppliedType(tycon, args.map(rangeToBounds))
            else {
              val loBuf, hiBuf = new mutable.ListBuffer[Type]
              // Given `C[A1, ..., An]` where sone A's are ranges, try to find
              // non-range arguments L1, ..., Ln and H1, ..., Hn such that
              // C[L1, ..., Ln] <: C[H1, ..., Hn] by taking the right limits of
              // ranges that appear in as co- or contravariant arguments.
              // Fail for non-variant argument ranges.
              // If successful, the L-arguments are in loBut, the H-arguments in hiBuf.
              // @return  operation succeeded for all arguments.
              def distributeArgs(args: List[Type], tparams: List[ParamInfo]): Boolean = args match {
                case Range(lo, hi) :: args1 =>
                  val v = tparams.head.paramVariance
                  if (v == 0) false
                  else {
                    if (v > 0) { loBuf += lo; hiBuf += hi }
                    else { loBuf += hi; hiBuf += lo }
                    distributeArgs(args1, tparams.tail)
                  }
                case arg :: args1 =>
                  loBuf += arg; hiBuf += arg
                  distributeArgs(args1, tparams.tail)
                case nil =>
                  true
              }
              if (distributeArgs(args, tp.typeParams))
                range(tp.derivedAppliedType(tycon, loBuf.toList),
                      tp.derivedAppliedType(tycon, hiBuf.toList))
              else range(tp.bottomType, tp.topType)
                // TODO: can we give a better bound than `topType`?
            }
          }
          else tp.derivedAppliedType(tycon, args)
      }
    override protected def derivedAndOrType(tp: AndOrType, tp1: Type, tp2: Type) =
      if (isRange(tp1) || isRange(tp2))
        if (tp.isAnd) range(lower(tp1) & lower(tp2), upper(tp1) & upper(tp2))
        else range(lower(tp1) | lower(tp2), upper(tp1) | upper(tp2))
      else tp.derivedAndOrType(tp1, tp2)

    override protected def derivedTypeArgRef(tp: TypeArgRef, prefix: Type): Type =
      if (isRange(prefix))
        tp.underlying match {
          case TypeBounds(lo, hi) => range(atVariance(-variance)(reapply(lo)), reapply(hi))
          case _ => range(tp.bottomType, tp.topType)
        }
      else tp.derivedTypeArgRef(prefix)

    override protected def derivedAnnotatedType(tp: AnnotatedType, underlying: Type, annot: Annotation) =
      underlying match {
        case Range(lo, hi) =>
          range(tp.derivedAnnotatedType(lo, annot), tp.derivedAnnotatedType(hi, annot))
        case _ =>
          if (underlying.isBottomType) underlying
          else tp.derivedAnnotatedType(underlying, annot)
      }
    override protected def derivedWildcardType(tp: WildcardType, bounds: Type) = {
      tp.derivedWildcardType(rangeToBounds(bounds))
    }

    override protected def derivedClassInfo(tp: ClassInfo, pre: Type): Type = {
      assert(!isRange(pre))
        // we don't know what to do here; this case has to be handled in subclasses
        // (typically by handling ClassInfo's specially, in case they can be encountered).
      tp.derivedClassInfo(pre)
    }

    override protected def derivedLambdaType(tp: LambdaType)(formals: List[tp.PInfo], restpe: Type): Type =
      restpe match {
        case Range(lo, hi) =>
          range(derivedLambdaType(tp)(formals, lo), derivedLambdaType(tp)(formals, hi))
        case _ =>
          tp.derivedLambdaType(tp.paramNames, formals, restpe)
      }

    protected def reapply(tp: Type): Type = apply(tp)
  }

  /** A range of possible types between lower bound `lo` and upper bound `hi`.
   *  Only used internally in `ApproximatingTypeMap`.
   */
  private case class Range(lo: Type, hi: Type) extends UncachedGroundType {
    assert(!lo.isInstanceOf[Range])
    assert(!hi.isInstanceOf[Range])

    override def toText(printer: Printer): Text =
      lo.toText(printer) ~ ".." ~ hi.toText(printer)
  }

  // ----- TypeAccumulators ----------------------------------------------------

  abstract class TypeAccumulator[T](implicit protected val ctx: Context)
  extends VariantTraversal with ((T, Type) => T) {

    protected def stopAtStatic = true

    def apply(x: T, tp: Type): T

    protected def applyToAnnot(x: T, annot: Annotation): T = x // don't go into annotations

    protected final def applyToPrefix(x: T, tp: NamedType) =
      atVariance(variance max 0)(this(x, tp.prefix)) // see remark on NamedType case in TypeMap

    def foldOver(x: T, tp: Type): T = tp match {
      case tp: TypeRef =>
        if (stopAtStatic && tp.symbol.isStatic) x
        else {
          val tp1 = tp.prefix.lookupRefined(tp.name)
          if (tp1.exists) this(x, tp1) else applyToPrefix(x, tp)
        }
      case tp: TermRef =>
        if (stopAtStatic && tp.currentSymbol.isStatic) x
        else applyToPrefix(x, tp)

      case _: ThisType
         | _: BoundType
         | NoPrefix => x

      case tp @ AppliedType(tycon, args) =>
        @tailrec def foldArgs(x: T, tparams: List[ParamInfo], args: List[Type]): T =
          if (args.isEmpty) {
            assert(tparams.isEmpty)
            x
          }
          else {
            val tparam = tparams.head
            val acc = args.head match {
              case arg: TypeBounds => this(x, arg)
              case arg => atVariance(variance * tparam.paramVariance)(this(x, arg))
            }
            foldArgs(acc, tparams.tail, args.tail)
          }
        foldArgs(this(x, tycon), tp.typeParams, args)

      case tp: RefinedType =>
        this(this(x, tp.parent), tp.refinedInfo)

      case bounds @ TypeBounds(lo, hi) =>
        if (lo eq hi) atVariance(variance * bounds.variance)(this(x, lo))
        else {
          variance = -variance
          val y = this(x, lo)
          variance = -variance
          this(y, hi)
        }

      case tp: RecType =>
        this(x, tp.parent)

      case ExprType(restpe) =>
        this(x, restpe)

      case tp: TypeVar =>
        this(x, tp.underlying)

      case SuperType(thistp, supertp) =>
        this(this(x, thistp), supertp)

      case tp @ ClassInfo(prefix, _, _, _, _) =>
        this(x, prefix)

      case tp @ HKApply(tycon, args) =>
        @tailrec def foldArgs(x: T, tparams: List[ParamInfo], args: List[Type]): T =
          if (args.isEmpty) {
            assert(tparams.isEmpty)
            x
          }
          else {
            val tparam = tparams.head
            val saved = variance
            variance *= tparam.paramVariance
            val acc =
              try this(x, args.head)
              finally variance = saved
            foldArgs(acc, tparams.tail, args.tail)
          }
        foldArgs(this(x, tycon), tp.typeParams, args)

      case tp: LambdaType =>
        variance = -variance
        val y = foldOver(x, tp.paramInfos)
        variance = -variance
        this(y, tp.resultType)

      case tp: AndOrType =>
        this(this(x, tp.tp1), tp.tp2)

      case tp: SkolemType =>
        this(x, tp.info)

      case tp @ TypeArgRef(prefix, _, _) =>
        val saved = variance
        variance = 0
        try this(x, prefix)
        finally variance = saved

      case AnnotatedType(underlying, annot) =>
        this(applyToAnnot(x, annot), underlying)

      case tp: WildcardType =>
        this(x, tp.optBounds)

      case tp: JavaArrayType =>
        this(x, tp.elemType)

      case tp: LazyRef =>
        this(x, tp.ref)

      case tp: ProtoType =>
        tp.fold(x, this)

      case _ => x
    }

    @tailrec final def foldOver(x: T, ts: List[Type]): T = ts match {
      case t :: ts1 => foldOver(apply(x, t), ts1)
      case nil => x
    }
  }

  abstract class TypeTraverser(implicit ctx: Context) extends TypeAccumulator[Unit] {
    def traverse(tp: Type): Unit
    def apply(x: Unit, tp: Type): Unit = traverse(tp)
    protected def traverseChildren(tp: Type) = foldOver((), tp)
  }

  class ExistsAccumulator(p: Type => Boolean, forceLazy: Boolean = true)(implicit ctx: Context) extends TypeAccumulator[Boolean] {
    override def stopAtStatic = false
    def apply(x: Boolean, tp: Type) =
      x || p(tp) || (forceLazy || !tp.isInstanceOf[LazyRef]) && foldOver(x, tp)
  }

  class ForeachAccumulator(p: Type => Unit, override val stopAtStatic: Boolean)(implicit ctx: Context) extends TypeAccumulator[Unit] {
    def apply(x: Unit, tp: Type): Unit = foldOver(p(tp), tp)
  }

  class NamedPartsAccumulator(p: NamedType => Boolean, excludeLowerBounds: Boolean = false)
    (implicit ctx: Context) extends TypeAccumulator[mutable.Set[NamedType]] {
    override def stopAtStatic = false
    def maybeAdd(x: mutable.Set[NamedType], tp: NamedType) = if (p(tp)) x += tp else x
    val seen: mutable.Set[Type] = mutable.Set()
    def apply(x: mutable.Set[NamedType], tp: Type): mutable.Set[NamedType] =
      if (seen contains tp) x
      else {
        seen += tp
        tp match {
          case tp: TermRef =>
            apply(foldOver(maybeAdd(x, tp), tp), tp.underlying)
          case tp: TypeRef =>
            foldOver(maybeAdd(x, tp), tp)
          case TypeBounds(lo, hi) =>
            if (!excludeLowerBounds) apply(x, lo)
            apply(x, hi)
          case tp: ThisType =>
            apply(x, tp.tref)
          case tp: ConstantType =>
            apply(x, tp.underlying)
          case tp: ParamRef =>
            apply(x, tp.underlying)
          case _ =>
            foldOver(x, tp)
        }
      }
  }

  //   ----- Name Filters --------------------------------------------------

  /** A name filter selects or discards a member name of a type `pre`.
   *  To enable efficient caching, name filters have to satisfy the
   *  following invariant: If `keep` is a name filter, and `pre` has
   *  class `C` as a base class, then
   *
   *    keep(pre, name)  implies  keep(C.this, name)
   */
  abstract class NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean
  }

  /** A filter for names of abstract types of a given type */
  object abstractTypeNameFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean =
      name.isTypeName && {
        val mbr = pre.nonPrivateMember(name)
        (mbr.symbol is Deferred) && mbr.info.isInstanceOf[RealTypeBounds]
      }
  }

  /** A filter for names of abstract types of a given type */
  object nonClassTypeNameFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean =
      name.isTypeName && {
        val mbr = pre.member(name)
        mbr.symbol.isType && !mbr.symbol.isClass
      }
  }

  /** A filter for names of deferred term definitions of a given type */
  object abstractTermNameFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean =
      name.isTermName && pre.nonPrivateMember(name).hasAltWith(_.symbol is Deferred)
  }

  object typeNameFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean = name.isTypeName
  }

  object fieldFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean =
      name.isTermName && (pre member name).hasAltWith(!_.symbol.is(Method))
  }

  object takeAllFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean = true
  }

  object implicitFilter extends NameFilter {
    /** A dummy filter method.
     *  Implicit filtering is handled specially in computeMemberNames, so
     *  no post-filtering is needed.
     */
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean = true
  }

  // ----- Exceptions -------------------------------------------------------------

  class TypeError(msg: String) extends Exception(msg)

  class MalformedType(pre: Type, denot: Denotation, absMembers: Set[Name])
    extends TypeError(
      s"malformed type: $pre is not a legal prefix for $denot because it contains abstract type member${if (absMembers.size == 1) "" else "s"} ${absMembers.mkString(", ")}")

  class MissingType(pre: Type, name: Name)(implicit ctx: Context) extends TypeError(
    i"""cannot resolve reference to type $pre.$name
       |the classfile defining the type might be missing from the classpath${otherReason(pre)}""") {
    if (ctx.debug) printStackTrace()
  }

  private def otherReason(pre: Type)(implicit ctx: Context): String = pre match {
    case pre: ThisType if pre.cls.givenSelfType.exists =>
      i"\nor the self type of $pre might not contain all transitive dependencies"
    case _ => ""
  }

  class CyclicReference private (val denot: SymDenotation)
    extends TypeError(s"cyclic reference involving $denot") {
    def toMessage(implicit ctx: Context) = CyclicReferenceInvolving(denot)
  }

  object CyclicReference {
    def apply(denot: SymDenotation)(implicit ctx: Context): CyclicReference = {
      val ex = new CyclicReference(denot)
      if (!(ctx.mode is Mode.CheckCyclic)) {
        cyclicErrors.println(ex.getMessage)
        for (elem <- ex.getStackTrace take 200)
          cyclicErrors.println(elem.toString)
      }
      ex
    }
  }

  class MergeError(msg: String, val tp1: Type, val tp2: Type) extends TypeError(msg)

  // ----- Debug ---------------------------------------------------------

  @sharable var debugTrace = false

  val watchList = List[String](
  ) map (_.toTypeName)

  def isWatched(tp: Type) = tp match {
    case TypeRef(_, name) => watchList contains name
    case _ => false
  }

  // ----- Decorator implicits --------------------------------------------

  implicit def decorateTypeApplications(tpe: Type): TypeApplications = new TypeApplications(tpe)
}
