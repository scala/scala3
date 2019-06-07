package dotty.tools
package dotc
package core

import util.common._
import Symbols._
import Flags._
import Names._
import StdNames._, NameOps._
import NameKinds.SkolemName
import Scopes._
import Constants._
import Contexts._
import Annotations._
import SymDenotations._
import Decorators._
import Denotations._
import Periods._
import util.Stats._
import util.SimpleIdentitySet
import reporting.diagnostic.Message
import ast.tpd._
import ast.TreeTypeMap
import printing.Texts._
import printing.Printer
import Hashable._
import Uniques._
import collection.{mutable, Seq}
import config.Config
import annotation.tailrec
import language.implicitConversions
import scala.util.hashing.{ MurmurHash3 => hashing }
import config.Printers.{core, typr}
import reporting.trace
import java.lang.ref.WeakReference

import scala.annotation.internal.sharable
import scala.annotation.threadUnsafe

object Types {

  @sharable private[this] var nextId = 0

  implicit def eqType: Eql[Type, Type] = Eql.derived

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
   *        |              +- AppliedType
   *        |              +- TypeBounds
   *        |              +- ExprType
   *        |              +- AnnotatedType
   *        |              +- TypeVar
   *        |              +- HKTypeLambda
   *        |              +- MatchType
   *        |
   *        +- GroundType -+- AndType
   *                       +- OrType
   *                       +- MethodOrPoly ---+-- PolyType
   *                                          +-- MethodType ---+- ImplicitMethodType
   *                                                            +- ContextualMethodType
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
  abstract class Type extends Hashable with printing.Showable {

// ----- Tests -----------------------------------------------------

//    // debug only: a unique identifier for a type
//    val uniqId = {
//      nextId = nextId + 1
//      if (nextId == 19555)
//        println("foo")
//      nextId
//    }

    /** A cache indicating whether the type was still provisional, last time we checked */
    @sharable private var mightBeProvisional = true

    /** Is this type still provisional? This is the case if the type contains, or depends on,
     *  uninstantiated type variables or type symbols that have the Provisional flag set.
     *  This is an antimonotonic property - once a type is not provisional, it stays so forever.
     */
    def isProvisional(implicit ctx: Context): Boolean = mightBeProvisional && testProvisional

    private def testProvisional(implicit ctx: Context) = {
      val accu = new TypeAccumulator[Boolean] {
        override def apply(x: Boolean, t: Type) =
          x || t.mightBeProvisional && {
            t.mightBeProvisional = t match {
              case t: TypeVar =>
                !t.inst.exists || apply(x, t.inst)
              case t: TypeRef =>
                (t: Type).mightBeProvisional = false // break cycles
                t.symbol.is(Provisional) ||
                apply(x, t.prefix) || {
                  t.info match {
                    case info: AliasingBounds => apply(x, info.alias)
                    case TypeBounds(lo, hi) => apply(apply(x, lo), hi)
                    case _ => false
                  }
                }
              case t: LazyRef =>
                !t.completed || apply(x, t.ref)
              case _ =>
                foldOver(x, t)
            }
            t.mightBeProvisional
          }
      }
      accu.apply(false, this)
    }

    /** Is this type different from NoType? */
    final def exists: Boolean = this.ne(NoType)

    /** This type, if it exists, otherwise `that` type */
    def orElse(that: => Type): Type = if (exists) this else that

    /** Is this type a value type? */
    final def isValueType: Boolean = this.isInstanceOf[ValueType]

    /** Is this a value type or a type lambda? */
    final def isValueTypeOrLambda: Boolean = isValueType || this.isInstanceOf[TypeLambda]

    /** Is this a value type or a wildcard? */
    final def isValueTypeOrWildcard: Boolean = isValueType || this.isInstanceOf[WildcardType]

    /** Does this type denote a stable reference (i.e. singleton type)?
      *
      * Like in isStableMember, "stability" means idempotence.
      * Rationale: If an expression has a stable type, the expression must be idempotent, so stable types
      * must be singleton types of stable expressions. */
    final def isStable(implicit ctx: Context): Boolean = stripTypeVar match {
      case tp: TermRef => tp.symbol.isStableMember && tp.prefix.isStable || tp.info.isStable
      case _: SingletonType | NoPrefix => true
      case tp: RefinedOrRecType => tp.parent.isStable
      case tp: ExprType => tp.resultType.isStable
      case tp: AnnotatedType => tp.parent.isStable
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
          case TypeAlias(tp) => tp.isRef(sym)
          case _ => this1.symbol eq sym
        }
      case this1: RefinedOrRecType =>
        this1.parent.isRef(sym)
      case this1: AppliedType =>
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
          if (sym.isClass) sym.derivesFrom(cls) else loop(tp.superType)
        case tp: AppliedType =>
          tp.superType.derivesFrom(cls)
        case tp: MatchType =>
          tp.bound.derivesFrom(cls) || tp.reduced.derivesFrom(cls)
        case tp: TypeProxy =>
          loop(tp.underlying)
        case tp: AndType =>
          loop(tp.tp1) || loop(tp.tp2)
        case tp: OrType =>
          loop(tp.tp1) && loop(tp.tp2)
        case tp: JavaArrayType =>
          cls == defn.ObjectClass
        case _ =>
          false
      }
      loop(this)
    }

    /** True iff `symd` is a denotation of a class type parameter and the reference
     *  `<this> . <symd>` is an actual argument reference, i.e. `this` is different
     *  from the ThisType of `symd`'s owner.
     */
    def isArgPrefixOf(symd: SymDenotation)(implicit ctx: Context): Boolean =
      symd.is(ClassTypeParam) && {
        this match {
          case tp: ThisType => tp.cls ne symd.owner
          case _ => true
        }
      }

    /** Is this type exactly Nothing (no vars, aliases, refinements etc allowed)? */
    def isBottomType(implicit ctx: Context): Boolean = this match {
      case tp: TypeRef => tp.symbol eq defn.NothingClass
      case _ => false
    }

      /** Is this type exactly Any (no vars, aliases, refinements etc allowed)? */
    def isTopType(implicit ctx: Context): Boolean = this match {
      case tp: TypeRef => tp.symbol eq defn.AnyClass
      case _ => false
    }

    /** Is this type a (possibly aliased) singleton type? */
    def isSingleton(implicit ctx: Context): Boolean = dealias.isInstanceOf[SingletonType]

    /** Is this type of kind `AnyKind`? */
    def hasAnyKind(implicit ctx: Context): Boolean = {
      @tailrec def loop(tp: Type): Boolean = tp match {
        case tp: TypeRef =>
          val sym = tp.symbol
          if (sym.isClass) sym == defn.AnyKindClass else loop(tp.superType)
        case tp: TypeProxy =>
          loop(tp.underlying)
        case _ =>
          false
      }
      loop(this)
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
    final def isError(implicit ctx: Context): Boolean = stripTypeVar.isInstanceOf[ErrorType]

    /** Is some part of the widened version of this type produced as a repair for an error? */
    def isErroneous(implicit ctx: Context): Boolean =
      widen.existsPart(_.isError, forceLazy = false)

    /** Does the type carry an annotation that is an instance of `cls`? */
    @tailrec final def hasAnnotation(cls: ClassSymbol)(implicit ctx: Context): Boolean = stripTypeVar match {
      case AnnotatedType(tp, annot) => (annot matches cls) || (tp hasAnnotation cls)
      case _ => false
    }

    /** Does this type have a supertype with an annotation satisfying given predicate `p`? */
    def derivesAnnotWith(p: Annotation => Boolean)(implicit ctx: Context): Boolean = this match {
      case tp: AnnotatedType => p(tp.annot) || tp.parent.derivesAnnotWith(p)
      case tp: TypeProxy => tp.superType.derivesAnnotWith(p)
      case AndType(l, r) => l.derivesAnnotWith(p) || r.derivesAnnotWith(p)
      case OrType(l, r) => l.derivesAnnotWith(p) && r.derivesAnnotWith(p)
      case _ => false
    }

    /** Does this type occur as a part of type `that`? */
    def occursIn(that: Type)(implicit ctx: Context): Boolean =
      that.existsPart(this == _)

    /** Does this type not refer to TypeParamRefs or uninstantiated TypeVars? */
    final def isGround(implicit ctx: Context): Boolean =
      (new isGroundAccumulator).apply(true, this)

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
    final def isTypeAlias: Boolean = this.isInstanceOf[TypeAlias]

    /** Is this a MethodType which is from Java? */
    def isJavaMethod: Boolean = false

    /** Is this a Method or PolyType which has implicit or contextual parameters? */
    def isImplicitMethod: Boolean = false

    /** Is this a Method or PolyType which has contextual parameters as first value parameter list? */
    def isContextualMethod: Boolean = false

    /** Is this a MethodType for which the parameters will not be used? */
    def isErasedMethod: Boolean = false

    /** Is this a match type or a higher-kinded abstraction of one?
     */
    def isMatch(implicit ctx: Context): Boolean = stripTypeVar.stripAnnots match {
      case _: MatchType => true
      case tp: HKTypeLambda => tp.resType.isMatch
      case _ => false
    }

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
        constant.tpe.classSymbol
      case tp: TypeRef =>
        val sym = tp.symbol
        if (sym.isClass) sym else tp.superType.classSymbol
      case tp: ClassInfo =>
        tp.cls
      case tp: SingletonType =>
        NoSymbol
      case tp: TypeProxy =>
        tp.underlying.classSymbol
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
        if (sym.isClass) sym.asClass :: Nil else tp.superType.classSymbols
      case tp: TypeProxy =>
        tp.underlying.classSymbols
      case AndType(l, r) =>
        l.classSymbols | r.classSymbols
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
        tp.underlying.decls
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
        tp.decls.denotsNamed(name).filterWithFlags(EmptyFlagConjunction, excluded).toDenot(NoPrefix)
      case tp: TypeProxy =>
        tp.underlying.findDecl(name, excluded)
      case err: ErrorType =>
        ctx.newErrorSymbol(classSymbol orElse defn.RootClass, name, err.msg)
      case _ =>
        NoDenotation
    }

    /** The member of this type with the given name  */
    final def member(name: Name)(implicit ctx: Context): Denotation = /*>|>*/ track("member") /*<|<*/ {
      memberBasedOnFlags(name, required = EmptyFlagConjunction, excluded = EmptyFlags)
    }

    /** The non-private member of this type with the given name. */
    final def nonPrivateMember(name: Name)(implicit ctx: Context): Denotation = track("nonPrivateMember") {
      memberBasedOnFlags(name, required = EmptyFlagConjunction, excluded = Flags.Private)
    }

    /** The member with given `name` and required and/or excluded flags */
    final def memberBasedOnFlags(name: Name, required: FlagConjunction = EmptyFlagConjunction, excluded: FlagSet = EmptyFlags)(implicit ctx: Context): Denotation = {
      // We need a valid prefix for `asSeenFrom`
      val pre = this match {
        case tp: ClassInfo => tp.appliedRef
        case _ => widenIfUnstable
      }
      findMember(name, pre, required, excluded)
    }

    /** Find member of this type with given `name`, all `required`
     *  flags and no `excluded` flag and produce a denotation that contains
     *  the type of the member as seen from given prefix `pre`.
     */
    final def findMember(name: Name, pre: Type, required: FlagConjunction = EmptyFlagConjunction, excluded: FlagSet = EmptyFlags)(implicit ctx: Context): Denotation = {
      @tailrec def go(tp: Type): Denotation = tp match {
        case tp: TermRef =>
          go (tp.underlying match {
            case mt: MethodType
            if mt.paramInfos.isEmpty && (tp.symbol is StableRealizable) => mt.resultType
            case tp1 => tp1
          })
        case tp: TypeRef =>
          tp.denot match {
            case d: ClassDenotation => d.findMember(name, pre, required, excluded)
            case d => go(d.info)
          }
        case tp: AppliedType =>
          tp.tycon match {
            case tc: TypeRef =>
              if (tc.symbol.isClass) go(tc)
              else {
                val normed = tp.tryNormalize
                go(if (normed.exists) normed else tp.superType)
              }
            case tc: HKTypeLambda =>
              goApplied(tp, tc)
            case _ =>
              go(tp.superType)
          }
        case tp: ThisType => // ??? inline
          goThis(tp)
        case tp: RefinedType =>
          if (name eq tp.refinedName) goRefined(tp) else go(tp.parent)
        case tp: RecType =>
          goRec(tp)
        case tp: TypeParamRef =>
          goParam(tp)
        case tp: SuperType =>
          goSuper(tp)
        case tp: MatchType =>
          val normed = tp.tryNormalize
          go(if (normed.exists) normed else tp.underlying)
        case tp: TypeProxy =>
          go(tp.underlying)
        case tp: ClassInfo =>
          tp.cls.findMember(name, pre, required, excluded)
        case AndType(l, r) =>
          goAnd(l, r)
        case tp: OrType =>
          // we need to keep the invariant that `pre <: tp`. Branch `union-types-narrow-prefix`
          // achieved that by narrowing `pre` to each alternative, but it led to merge errors in
          // lots of places. The present strategy is instead of widen `tp` using `join` to be a
          // supertype of `pre`.
          go(tp.join)
        case tp: JavaArrayType =>
          defn.ObjectType.findMember(name, pre, required, excluded)
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
        val pinfo = pdenot.info
        val rinfo = tp.refinedInfo
        if (name.isTypeName && !pinfo.isInstanceOf[ClassInfo]) { // simplified case that runs more efficiently
          val jointInfo =
            if (ctx.base.pendingMemberSearches.contains(name)) pinfo safe_& rinfo
            else pinfo recoverable_& rinfo
          pdenot.asSingleDenotation.derivedSingleDenotation(pdenot.symbol, jointInfo)
        } else {
          pdenot & (
            new JointRefDenotation(NoSymbol, rinfo, Period.allInRun(ctx.runId)),
            pre,
            safeIntersection = ctx.base.pendingMemberSearches.contains(name))
        }
      }

      def goApplied(tp: AppliedType, tycon: HKTypeLambda) =
        go(tycon.resType).mapInfo(info =>
          tycon.derivedLambdaAbstraction(tycon.paramNames, tycon.paramInfos, info).appliedTo(tp.args))

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
        go(l) & (go(r), pre, safeIntersection = ctx.base.pendingMemberSearches.contains(name))
      }

      val recCount = ctx.base.findMemberCount
      if (recCount >= Config.LogPendingFindMemberThreshold)
        ctx.base.pendingMemberSearches = name :: ctx.base.pendingMemberSearches
      ctx.base.findMemberCount = recCount + 1
      try go(this)
      catch {
        case ex: Throwable =>
          core.println(s"findMember exception for $this member $name, pre = $pre, recCount = $recCount")

          def showPrefixSafely(pre: Type)(implicit ctx: Context): String = pre.stripTypeVar match {
            case pre: TermRef => i"${pre.termSymbol.name}."
            case pre: TypeRef => i"${pre.typeSymbol.name}#"
            case pre: TypeProxy => showPrefixSafely(pre.underlying)
            case _ => if (pre.typeSymbol.exists) i"${pre.typeSymbol.name}#" else "."
          }

          handleRecursive("find-member", i"${showPrefixSafely(pre)}$name", ex)
      }
      finally {
        if (recCount >= Config.LogPendingFindMemberThreshold)
          ctx.base.pendingMemberSearches = ctx.base.pendingMemberSearches.tail
        ctx.base.findMemberCount = recCount
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
        tp.underlying.memberNames(keepOnly, pre)
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

    /** The set of type alias members of this type */
    final def typeAliasMembers(implicit ctx: Context): Seq[SingleDenotation] = track("typeAlias") {
      memberDenots(typeAliasNameFilter,
          (name, buf) => buf += member(name).asSingleDenotation)
    }

    /** The set of type members of this type */
    final def typeMembers(implicit ctx: Context): Seq[SingleDenotation] = track("typeMembers") {
      memberDenots(typeNameFilter,
          (name, buf) => buf += member(name).asSingleDenotation)
    }

    /** The set of implicit term members of this type
     *  @param kind   A subset of {Implicit, Implied} that specifies what kind of implicit should
     *                be returned
     */
    final def implicitMembers(kind: FlagSet)(implicit ctx: Context): List[TermRef] = track("implicitMembers") {
      memberDenots(implicitFilter,
          (name, buf) => buf ++= member(name).altsWith(_.is(ImplicitOrImpliedOrGivenTerm & kind)))
        .toList.map(d => TermRef(this, d.symbol.asTerm))
    }

    /** The set of member classes of this type */
    final def memberClasses(implicit ctx: Context): Seq[SingleDenotation] = track("memberClasses") {
      memberDenots(typeNameFilter,
        (name, buf) => buf ++= member(name).altsWith(x => x.isClass))
    }

    final def fields(implicit ctx: Context): Seq[SingleDenotation] = track("fields") {
      memberDenots(fieldFilter,
        (name, buf) => buf ++= member(name).altsWith(x => !x.is(Method)))
    }

    /** The set of members of this type that have all of `required` flags but none of `excluded` flags set. */
    final def membersBasedOnFlags(required: FlagConjunction, excluded: FlagSet)(implicit ctx: Context): Seq[SingleDenotation] = track("membersBasedOnFlags") {
      memberDenots(takeAllFilter,
        (name, buf) => buf ++= memberBasedOnFlags(name, required, excluded).alternatives)
    }

    /** All members of this type. Warning: this can be expensive to compute! */
    final def allMembers(implicit ctx: Context): Seq[SingleDenotation] = track("allMembers") {
      memberDenots(takeAllFilter, (name, buf) => buf ++= member(name).alternatives)
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

    final def frozen_=:=(that: Type)(implicit ctx: Context): Boolean =
      ctx.typeComparer.isSameTypeWhenFrozen(this, that)

    /** Is this type a primitive value type which can be widened to the primitive value type `that`? */
    def isValueSubType(that: Type)(implicit ctx: Context): Boolean = widen match {
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

    def relaxed_<:<(that: Type)(implicit ctx: Context): Boolean =
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
          ((this1 `ne` this) || (that1 `ne` that)) && this1.overrides(that1, matchLoosely = false)
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

    /** The basetype of this type with given class symbol, NoType if `base` is not a class. */
    final def baseType(base: Symbol)(implicit ctx: Context): Type = /*trace(s"$this baseType $base")*/ /*>|>*/ track("base type") /*<|<*/ {
      base.denot match {
        case classd: ClassDenotation => classd.baseTypeOf(this)
        case _ => NoType
      }
    }

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
     *  pos/i536 demonstrates that the infinite loop can also involve lower bounds.
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

    def rewrapAnnots(tp: Type)(implicit ctx: Context): Type = tp.stripTypeVar match {
      case AnnotatedType(tp1, annot) => AnnotatedType(rewrapAnnots(tp1), annot)
      case _ => this
    }

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
    final def widenSingleton(implicit ctx: Context): Type = stripTypeVar.stripAnnots match {
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
      case tp: TermRef if !tp.symbol.isStableMember => tp.underlying.widenIfUnstable
      case _ => this
    }

    /** If this is a skolem, its underlying type, otherwise the type itself */
    final def widenSkolem(implicit ctx: Context): Type = this match {
      case tp: SkolemType => tp.underlying
      case _ => this
    }

    /** Widen this type and if the result contains embedded union types, replace
     *  them by their joins.
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
    def widenUnion(implicit ctx: Context): Type = widen match {
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
      case tp =>
        tp
    }

    /** Widen all top-level singletons reachable by dealiasing
     *  and going to the operands of & and |.
     *  Overridden and cached in OrType.
     */
    def widenSingletons(implicit ctx: Context): Type = dealias match {
      case tp: SingletonType =>
        tp.widen
      case tp: OrType =>
        val tp1w = tp.widenSingletons
        if (tp1w eq tp) this else tp1w
      case tp: AndType =>
        val tp1w = tp.tp1.widenSingletons
        val tp2w = tp.tp2.widenSingletons
        if ((tp.tp1 eq tp1w) && (tp.tp2 eq tp2w)) this else tp1w & tp2w
      case _ =>
        this
    }

    /** If this type is an alias of a disjunction of stable singleton types,
     *  these types as a set, otherwise the empty set.
     *  Overridden and cached in OrType.
     */
    def atoms(implicit ctx: Context): Set[Type] = dealias match {
      case tp: SingletonType if tp.isStable =>
        def normalize(tp: Type): Type = tp match {
          case tp: SingletonType =>
            tp.underlying.dealias match {
              case tp1: SingletonType => normalize(tp1)
              case _ =>
                tp match {
                  case tp: TermRef => tp.derivedSelect(normalize(tp.prefix))
                  case _ => tp
                }
            }
          case _ => tp
        }
        Set.empty + normalize(tp)
      case tp: OrType => tp.atoms
      case tp: AndType => tp.tp1.atoms & tp.tp2.atoms
      case _ => Set.empty
    }

    private def dealias1(keep: AnnotatedType => Context => Boolean)(implicit ctx: Context): Type = this match {
      case tp: TypeRef =>
        if (tp.symbol.isClass) tp
        else tp.info match {
          case TypeAlias(alias) => alias.dealias1(keep)
          case _ => tp
        }
      case app @ AppliedType(tycon, args) =>
        val tycon1 = tycon.dealias1(keep)
        if (tycon1 ne tycon) app.superType.dealias1(keep)
        else this
      case tp: TypeVar =>
        val tp1 = tp.instanceOpt
        if (tp1.exists) tp1.dealias1(keep) else tp
      case tp: AnnotatedType =>
        val tp1 = tp.parent.dealias1(keep)
        if (keep(tp)(ctx)) tp.derivedAnnotatedType(tp1, tp.annot) else tp1
      case tp: LazyRef =>
        tp.ref.dealias1(keep)
      case _ => this
    }

    /** Follow aliases and dereferences LazyRefs, annotated types and instantiated
     *  TypeVars until type is no longer alias type, annotated type, LazyRef,
     *  or instantiated type variable.
     */
    final def dealias(implicit ctx: Context): Type = dealias1(keepNever)

    /** Follow aliases and dereferences LazyRefs and instantiated TypeVars until type
     *  is no longer alias type, LazyRef, or instantiated type variable.
     *  Goes through annotated types and rewraps annotations on the result.
     */
    final def dealiasKeepAnnots(implicit ctx: Context): Type = dealias1(keepAlways)

    /** Like `dealiasKeepAnnots`, but keeps only refining annotations */
    final def dealiasKeepRefiningAnnots(implicit ctx: Context): Type = dealias1(keepIfRefining)

    /** The result of normalization using `tryNormalize`, or the type itself if
     *  tryNormlize yields NoType
     */
    final def normalized(implicit ctx: Context): Type = {
      val normed = tryNormalize
      if (normed.exists) normed else this
    }

    /** If this type can be normalized at the top-level by rewriting match types
     *  of S[n] types, the result after applying all toplevel normalizations,
     *  otherwise NoType
     */
    def tryNormalize(implicit ctx: Context): Type = NoType

    private def widenDealias1(keep: AnnotatedType => Context => Boolean)(implicit ctx: Context): Type = {
      val res = this.widen.dealias1(keep)
      if (res eq this) res else res.widenDealias1(keep)
    }

    /** Perform successive widenings and dealiasings until none can be applied anymore */
    final def widenDealias(implicit ctx: Context): Type = widenDealias1(keepNever)

    /** Perform successive widenings and dealiasings while rewrapping annotations, until none can be applied anymore */
    final def widenDealiasKeepAnnots(implicit ctx: Context): Type = widenDealias1(keepAlways)

    /** Perform successive widenings and dealiasings while rewrapping refining annotations, until none can be applied anymore */
    final def widenDealiasKeepRefiningAnnots(implicit ctx: Context): Type = widenDealias1(keepIfRefining)

    /** Widen from constant type to its underlying non-constant
     *  base type.
     */
    final def deconst(implicit ctx: Context): Type = stripTypeVar match {
      case tp: ConstantType => tp.value.tpe
      case _ => this
    }

    /** Dealias, and if result is a dependent function type, drop the `apply` refinement. */
    final def dropDependentRefinement(implicit ctx: Context): Type = dealias match {
      case RefinedType(parent, nme.apply, _) => parent
      case tp => tp
    }

    /** The type constructor of an applied type, otherwise the type itself */
    final def typeConstructor(implicit ctx: Context): Type = this match {
      case AppliedType(tycon, _) => tycon
      case _ => this
    }

    /** If this is a (possibly aliased, annotated, and/or parameterized) reference to
     *  a class, the class type ref, otherwise NoType.
     *  @param  refinementOK   If `true` we also skip refinements.
     */
    def underlyingClassRef(refinementOK: Boolean)(implicit ctx: Context): Type = dealias match {
      case tp: TypeRef =>
        if (tp.symbol.isClass) tp
        else if (tp.symbol.isAliasType) tp.underlying.underlyingClassRef(refinementOK)
        else NoType
      case tp: AppliedType =>
        if (tp.tycon.isLambdaSub) NoType
        else tp.superType.underlyingClassRef(refinementOK)
      case tp: AnnotatedType =>
        tp.underlying.underlyingClassRef(refinementOK)
      case tp: RefinedType =>
        if (refinementOK) tp.underlying.underlyingClassRef(refinementOK) else NoType
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
    def underlyingIfProxy(implicit ctx: Context): Type = this match {
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
    def select(name: Name)(implicit ctx: Context): Type =
      NamedType(this, name, member(name)).reduceProjection

    /** The type <this . name> with given denotation, reduced if possible. */
    def select(name: Name, denot: Denotation)(implicit ctx: Context): Type =
      NamedType(this, name, denot).reduceProjection

    /** The type <this . sym>, reduced if possible */
    def select(sym: Symbol)(implicit ctx: Context): Type =
      NamedType(this, sym).reduceProjection

    def select(name: TermName)(implicit ctx: Context): TermRef =
      TermRef(this, name, member(name))

    def select(name: TermName, sig: Signature)(implicit ctx: Context): TermRef =
      TermRef(this, name, member(name).atSignature(sig, relaxed = !ctx.erasedTypes))

// ----- Access to parts --------------------------------------------

    /** The normalized prefix of this type is:
     *  For an alias type, the normalized prefix of its alias
     *  For all other named type and class infos: the prefix.
     *  Inherited by all other type proxies.
     *  `NoType` for all other types.
     */
    @tailrec final def normalizedPrefix(implicit ctx: Context): Type = this match {
      case tp: NamedType =>
        if (tp.symbol.info.isTypeAlias) tp.info.normalizedPrefix else tp.prefix
      case tp: ClassInfo =>
        tp.prefix
      case tp: TypeProxy =>
        tp.underlying.normalizedPrefix
      case _ =>
        NoType
    }

    /** The full parent types, including all type arguments */
    def parents(implicit ctx: Context): List[Type] = this match {
      case tp @ AppliedType(tycon, args) if tycon.typeSymbol.isClass =>
        tycon.parents.map(_.subst(tycon.typeSymbol.typeParams, args))
      case tp: TypeRef =>
        if (tp.info.isInstanceOf[TempClassInfo]) {
          tp.recomputeDenot()
          assert(!tp.info.isInstanceOf[TempClassInfo])
        }
        tp.info.parents
      case tp: TypeProxy =>
        tp.superType.parents
      case _ => Nil
    }

    /** The first parent of this type, AnyRef if list of parents is empty */
    def firstParent(implicit ctx: Context): Type = parents match {
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
    def loBound: Type = this match {
      case tp: TypeBounds => tp.lo
      case _ => this
    }

    /** The upper bound of a TypeBounds type, the type itself otherwise */
    def hiBound: Type = this match {
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

    /** If this is an ignored proto type, its underlying type, otherwise the type itself */
    def revealIgnored: Type = this

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

    /** Substitute all occurrences of symbols in `from` by corresponding types in `to`.
     *  Unlike for `subst`, the `to` types can be type bounds. A TypeBounds target
     *  will be replaced by range that gets absorbed in an approximating type map.
     */
    final def substApprox(from: List[Symbol], to: List[Type])(implicit ctx: Context): Type =
      new ctx.SubstApproxMap(from, to).apply(this)

// ----- misc -----------------------------------------------------------

    /** Turn type into a function type.
     *  @pre this is a method type without parameter dependencies.
     *  @param dropLast  The number of trailing parameters that should be dropped
     *                   when forming the function type.
     */
    def toFunctionType(dropLast: Int = 0)(implicit ctx: Context): Type = this match {
      case mt: MethodType if !mt.isParamDependent =>
        val formals1 = if (dropLast == 0) mt.paramInfos else mt.paramInfos dropRight dropLast
        val isContextual = mt.isContextualMethod && !ctx.erasedTypes
        val isErased = mt.isErasedMethod && !ctx.erasedTypes
        val result1 = mt.nonDependentResultApprox match {
          case res: MethodType => res.toFunctionType()
          case res => res
        }
        val funType = defn.FunctionOf(
          formals1 mapConserve (_.underlyingIfRepeated(mt.isJavaMethod)),
          result1, isContextual, isErased)
        if (mt.isResultDependent) RefinedType(funType, nme.apply, mt)
        else funType
    }

    /** The signature of this type. This is by default NotAMethod,
     *  but is overridden for PolyTypes, MethodTypes, and TermRef types.
     *  (the reason why we deviate from the "final-method-with-pattern-match-in-base-class"
     *   pattern is that method signatures use caching, so encapsulation
     *   is improved using an OO scheme).
     */
    def signature(implicit ctx: Context): Signature = Signature.NotAMethod

    /** Drop annotation of given `cls` from this type */
    def dropAnnot(cls: Symbol)(implicit ctx: Context): Type = stripTypeVar match {
      case self @ AnnotatedType(pre, annot) =>
        if (annot.symbol eq cls) pre
        else self.derivedAnnotatedType(pre.dropAnnot(cls), annot)
      case _ =>
        this
    }

    def dropRepeatedAnnot(implicit ctx: Context): Type = dropAnnot(defn.RepeatedAnnot)

    def annotatedToRepeated(implicit ctx: Context): Type = this match {
      case tp @ ExprType(tp1) => tp.derivedExprType(tp1.annotatedToRepeated)
      case AnnotatedType(tp, annot) if annot matches defn.RepeatedAnnot =>
        val typeSym = tp.typeSymbol.asClass
        assert(typeSym == defn.SeqClass || typeSym == defn.ArrayClass)
        tp.translateParameterized(typeSym, defn.RepeatedParamClass)
      case _ => this
    }

    /** The set of distinct symbols referred to by this type, after all aliases are expanded */
    def coveringSet(implicit ctx: Context): Set[Symbol] =
      (new CoveringSetAccumulator).apply(Set.empty[Symbol], this)

    /** The number of applications and refinements in this type, after all aliases are expanded */
    def typeSize(implicit ctx: Context): Int =
      (new TypeSizeAccumulator).apply(0, this)

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
    def simplified(implicit ctx: Context): Type = ctx.simplify(this, null)

    /** Compare `this == that`, assuming corresponding binders in `bs` are equal.
     *  The normal `equals` should be equivalent to `equals(that, null`)`.
     *  We usually override `equals` when we override `iso` except if the
     *  `equals` comes from a case class, so it already has the right definition anyway.
     */
    final def equals(that: Any, bs: BinderPairs): Boolean =
      (this `eq` that.asInstanceOf[AnyRef]) || this.iso(that, bs)

    /** Is `this` isomorphic to `that`, assuming pairs of matching binders `bs`?
     *  It is assumed that `this.ne(that)`.
     */
    protected def iso(that: Any, bs: BinderPairs): Boolean = this.equals(that)

    /** Equality used for hash-consing; uses `eq` on all recursive invocations,
     *  except where a BindingType is involved. The latter demand a deep isomorphism check.
     */
    def eql(that: Type): Boolean = this.equals(that)

    /** customized hash code of this type.
     *  NotCached for uncached types. Cached types
     *  compute hash and use it as the type's hashCode.
     */
    def hash: Int

    /** Compute hashcode relative to enclosing binders `bs` */
    def computeHash(bs: Binders): Int

    /** Is the `hash` of this type the same for all possible sequences of enclosing binders? */
    def stableHash: Boolean = true

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

    /** Same as superType, except that opaque types are treated as transparent aliases */
    def translucentSuperType(implicit ctx: Context): Type = superType
  }

  // Every type has to inherit one of the following four abstract type classes.,
  // which determine whether the type is cached, and whether
  // it is a proxy of some other type. The duplication in their methods
  // is for efficiency.

  /**  Instances of this class are cached and are not proxies. */
  abstract class CachedGroundType extends Type with CachedType {
    private[this] var myHash = HashUnknown
    final def hash: Int = {
      if (myHash == HashUnknown) {
        myHash = computeHash(null)
        assert(myHash != HashUnknown)
      }
      myHash
    }
    override final def hashCode: Int =
      if (hash == NotCached) System.identityHashCode(this) else hash
  }

  /**  Instances of this class are cached and are proxies. */
  abstract class CachedProxyType extends TypeProxy with CachedType {
    protected[this] var myHash: Int = HashUnknown
    final def hash: Int = {
      if (myHash == HashUnknown) {
        myHash = computeHash(null)
        assert(myHash != HashUnknown)
      }
      myHash
    }
    override final def hashCode: Int =
      if (hash == NotCached) System.identityHashCode(this) else hash
  }

  /**  Instances of this class are uncached and are not proxies. */
  abstract class UncachedGroundType extends Type {
    final def hash: Int = NotCached
    final def computeHash(bs: Binders): Int = NotCached
    if (monitored) {
      record(s"uncachable")
      record(s"uncachable: $getClass")
    }
  }

  /**  Instances of this class are uncached and are proxies. */
  abstract class UncachedProxyType extends TypeProxy {
    final def hash: Int = NotCached
    final def computeHash(bs: Binders): Int = NotCached
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

  /** A marker trait for types that can be types of values or that are higher-kinded  */
  trait ValueType extends ValueTypeOrProto

  /** A marker trait for types that are guaranteed to contain only a
   *  single non-null value (they might contain null in addition).
   */
  trait SingletonType extends TypeProxy with ValueType {
    def isOverloaded(implicit ctx: Context): Boolean = false
  }

  /** A trait for types that bind other types that refer to them.
   *  Instances are: LambdaType, RecType.
   */
  trait BindingType extends Type {

    /** If this type is in `bs`, a hashcode based on its position in `bs`.
     *  Otherise the standard identity hash.
     */
    override def identityHash(bs: Binders): Int = {
      def recur(n: Int, tp: BindingType, rest: Binders): Int =
        if (this `eq` tp) finishHash(hashing.mix(hashSeed, n), 1)
        else if (rest == null) System.identityHashCode(this)
        else recur(n + 1, rest.tp, rest.next)
      avoidSpecialHashes(
        if (bs == null) System.identityHashCode(this)
        else recur(1, bs.tp, bs.next))
    }

    def equalBinder(that: BindingType, bs: BinderPairs): Boolean =
      (this `eq` that) || bs != null && bs.matches(this, that)
  }

  /** A trait for proto-types, used as expected types in typer */
  trait ProtoType extends Type {
    def isMatchedBy(tp: Type, keepConstraint: Boolean = false)(implicit ctx: Context): Boolean
    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context): T
    def map(tm: TypeMap)(implicit ctx: Context): ProtoType

    /** If this prototype captures a context, the same prototype except that the result
     *  captures the given context `ctx`.
     */
    def withContext(ctx: Context): ProtoType = this
  }

  /** Implementations of this trait cache the results of `narrow`. */
  trait NarrowCached extends Type {
    private[this] var myNarrow: TermRef = null
    override def narrow(implicit ctx: Context): TermRef = {
      if (myNarrow eq null) myNarrow = super.narrow
      myNarrow
    }
  }

// --- NamedTypes ------------------------------------------------------------------

  abstract class NamedType extends CachedProxyType with ValueType with SignatureCachingType { self =>

    type ThisType >: this.type <: NamedType
    type ThisName <: Name

    val prefix: Type
    def designator: Designator
    protected def designator_=(d: Designator): Unit

    assert(prefix.isValueType || (prefix eq NoPrefix), s"invalid prefix $prefix")

    private[this] var myName: Name = null
    private[this] var lastDenotation: Denotation = null
    private[this] var lastSymbol: Symbol = null
    private[this] var checkedPeriod: Period = Nowhere
    private[this] var myStableHash: Byte = 0

    // Invariants:
    // (1) checkedPeriod != Nowhere  =>  lastDenotation != null
    // (2) lastDenotation != null    =>  lastSymbol != null
    // (3) mySigRunId != NoRunId     =>  mySig != null

    def isType: Boolean = isInstanceOf[TypeRef]
    def isTerm: Boolean = isInstanceOf[TermRef]

    /** If designator is a name, this name. Otherwise, the original name
     *  of the designator symbol.
     */
    final def name(implicit ctx: Context): ThisName = {
      if (myName == null) myName = computeName
      myName.asInstanceOf[ThisName]
    }

    private def computeName: Name = designator match {
      case name: Name => name
      case sym: Symbol => sym.originDenotation.name
    }

    /** The signature of the last known denotation, or if there is none, the
     *  signature of the symbol
     */
    protected def computeSignature(implicit ctx: Context): Signature = {
      val lastd = lastDenotation
      if (lastd != null) lastd.signature
      else symbol.asSeenFrom(prefix).signature
    }

    /** The signature of the current denotation if it is known without forcing.
     *  Otherwise the signature of the current symbol if it is known without forcing.
     *  Otherwise NotAMethod.
     */
    private def currentSignature(implicit ctx: Context): Signature =
      if (ctx.runId == mySignatureRunId) mySignature
      else {
        val lastd = lastDenotation
        if (lastd != null) lastd.signature
        else {
          val sym = currentSymbol
          if (sym.exists) sym.asSeenFrom(prefix).signature
          else Signature.NotAMethod
        }
      }

    final def symbol(implicit ctx: Context): Symbol =
      // We can rely on checkedPeriod (unlike in the definition of `denot` below)
      // because SymDenotation#installAfter never changes the symbol
      if (checkedPeriod == ctx.period) lastSymbol else computeSymbol

    private def computeSymbol(implicit ctx: Context): Symbol =
      designator match {
        case sym: Symbol =>
          if (sym.isValidInCurrentRun) sym else denot.symbol
        case name =>
          (if (denotationIsCurrent) lastDenotation else denot).symbol
      }

    /** There is a denotation computed which is valid (somewhere in) the
     *  current run.
     */
    def denotationIsCurrent(implicit ctx: Context): Boolean =
      lastDenotation != null && lastDenotation.validFor.runId == ctx.runId

    /** If the reference is symbolic or the denotation is current, its symbol, otherwise NoDenotation.
     *
     *  Note: This operation does not force the denotation, and is therefore
     *  timing dependent. It should only be used if the outcome of the
     *  essential computation does not depend on the symbol being present or not.
     *  It's currently used to take an optimized path in substituters and
     *  type accumulators, as well as to be safe in diagnostic printing.
     *  Normally, it's better to use `symbol`, not `currentSymbol`.
     */
    final def currentSymbol(implicit ctx: Context): Symbol = designator match {
      case sym: Symbol => sym
      case _ => if (denotationIsCurrent) lastDenotation.symbol else NoSymbol
    }

    /** Retrieves currently valid symbol without necessarily updating denotation.
     *  Assumes that symbols do not change between periods in the same run.
     *  Used to get the class underlying a ThisType.
     */
    private[Types] def stableInRunSymbol(implicit ctx: Context): Symbol = {
      if (checkedPeriod.runId == ctx.runId) lastSymbol
      else symbol
    }

    def info(implicit ctx: Context): Type = denot.info

    /** The denotation currently denoted by this type */
    final def denot(implicit ctx: Context): Denotation = {
      val now = ctx.period
      // Even if checkedPeriod == now we still need to recheck lastDenotation.validFor
      // as it may have been mutated by SymDenotation#installAfter
      if (checkedPeriod != Nowhere && lastDenotation.validFor.contains(now)) {
        checkedPeriod = now
        lastDenotation
      }
      else computeDenot
    }

    private def computeDenot(implicit ctx: Context): Denotation = {

      def finish(d: Denotation) = {
        if (d.exists)
          // Avoid storing NoDenotations in the cache - we will not be able to recover from
          // them. The situation might arise that a type has NoDenotation in some later
          // phase but a defined denotation earlier (e.g. a TypeRef to an abstract type
          // is undefined after erasure.) We need to be able to do time travel back and
          // forth also in these cases.
          setDenot(d)
        d
      }

      def fromDesignator = designator match {
        case name: Name =>
          val sym = lastSymbol
          val allowPrivate = sym == null || (sym == NoSymbol) || sym.lastKnownDenotation.flagsUNSAFE.is(Private)
          finish(memberDenot(name, allowPrivate))
        case sym: Symbol =>
          val symd = sym.lastKnownDenotation
          if (symd.validFor.runId != ctx.runId && !ctx.stillValid(symd))
            finish(memberDenot(symd.initial.name, allowPrivate = false))
          else if (prefix.isArgPrefixOf(symd))
            finish(argDenot(sym.asType))
          else if (infoDependsOnPrefix(symd, prefix)) {
            if (!symd.isClass && symd.is(Opaque, butNot = Deferred)) symd.normalizeOpaque()
            finish(memberDenot(symd.initial.name, allowPrivate = symd.is(Private)))
          }
          else
            finish(symd.current)
      }

      lastDenotation match {
        case lastd0: SingleDenotation =>
          val lastd = lastd0.skipRemoved
          if (lastd.validFor.runId == ctx.runId && (checkedPeriod != Nowhere)) finish(lastd.current)
          else lastd match {
            case lastd: SymDenotation =>
              if (ctx.stillValid(lastd) && (checkedPeriod != Nowhere)) finish(lastd.current)
              else finish(memberDenot(lastd.initial.name, allowPrivate = false))
            case _ =>
              fromDesignator
          }
        case _ => fromDesignator
      }
    }

    private def disambiguate(d: Denotation)(implicit ctx: Context): Denotation =
      disambiguate(d, currentSignature)

    private def disambiguate(d: Denotation, sig: Signature)(implicit ctx: Context): Denotation =
      if (sig != null)
        d.atSignature(sig, relaxed = !ctx.erasedTypes) match {
          case d1: SingleDenotation => d1
          case d1 =>
            d1.atSignature(sig, relaxed = false) match {
              case d2: SingleDenotation => d2
              case d2 => d2.suchThat(currentSymbol.eq).orElse(d2)
            }
        }
      else d

    private def memberDenot(name: Name, allowPrivate: Boolean)(implicit ctx: Context): Denotation = {
      var d = memberDenot(prefix, name, allowPrivate)
      if (!d.exists && !allowPrivate && ctx.mode.is(Mode.Interactive))
        // In the IDE we might change a public symbol to private, and would still expect to find it.
        d = memberDenot(prefix, name, true)
      if (!d.exists && ctx.phaseId > FirstPhaseId && lastDenotation.isInstanceOf[SymDenotation])
        // name has changed; try load in earlier phase and make current
        d = memberDenot(name, allowPrivate)(ctx.withPhase(ctx.phaseId - 1)).current
      if (d.isOverloaded)
        d = disambiguate(d)
      d
    }

    private def memberDenot(prefix: Type, name: Name, allowPrivate: Boolean)(implicit ctx: Context): Denotation =
      if (allowPrivate) prefix.member(name) else prefix.nonPrivateMember(name)

    private def argDenot(param: TypeSymbol)(implicit ctx: Context): Denotation = {
      val cls = param.owner
      val args = prefix.baseType(cls).argInfos
      val typeParams = cls.typeParams

      def concretize(arg: Type, tparam: TypeSymbol) = arg match {
        case arg: TypeBounds => TypeRef(prefix, tparam)
        case arg => arg
      }
      val concretized = args.zipWithConserve(typeParams)(concretize)

      def rebase(arg: Type) = arg.subst(typeParams, concretized)

      val idx = typeParams.indexOf(param)

      if (idx < args.length) {
        val argInfo = args(idx) match {
          case arg: TypeBounds =>
            val v = param.paramVariance
            val pbounds = param.paramInfo
            if (v > 0 && pbounds.loBound.dealiasKeepAnnots.isBottomType) TypeAlias(arg.hiBound & rebase(pbounds.hiBound))
            else if (v < 0 && pbounds.hiBound.dealiasKeepAnnots.isTopType) TypeAlias(arg.loBound | rebase(pbounds.loBound))
            else arg recoverable_& rebase(pbounds)
          case arg => TypeAlias(arg)
        }
        param.derivedSingleDenotation(param, argInfo)
      }
      else {
        if (!ctx.reporter.errorsReported)
          throw new TypeError(
            i"""bad parameter reference $this at ${ctx.phase}
               |the parameter is ${param.showLocated} but the prefix $prefix
               |does not define any corresponding arguments.""")
        NoDenotation
      }
    }

    /** Reload denotation by computing the member with the reference's name as seen
     *  from the reference's prefix.
     */
    def recomputeDenot()(implicit ctx: Context): Unit =
      setDenot(memberDenot(name, allowPrivate = !symbol.exists || symbol.is(Private)))

    private def setDenot(denot: Denotation)(implicit ctx: Context): Unit = {
      if (Config.checkNoDoubleBindings)
        if (ctx.settings.YnoDoubleBindings.value)
          checkSymAssign(denot.symbol)

      lastDenotation = denot
      lastSymbol = denot.symbol
      checkedPeriod = if (prefix.isProvisional) Nowhere else ctx.period
      designator match {
        case sym: Symbol if designator ne lastSymbol =>
          designator = lastSymbol.asInstanceOf[Designator{ type ThisName = self.ThisName }]
        case _ =>
      }
      checkDenot()
    }

    private def checkDenot()(implicit ctx: Context) = {}

    private def checkSymAssign(sym: Symbol)(implicit ctx: Context) = {
      def selfTypeOf(sym: Symbol) =
        if (sym.isClass) sym.asClass.givenSelfType else NoType
      assert(
        (lastSymbol eq sym)
        ||
        (lastSymbol eq null)
        ||
        !denotationIsCurrent
        ||
        lastSymbol.infoOrCompleter.isInstanceOf[ErrorType]
        ||
        !sym.exists
        ||
        !lastSymbol.exists
        ||
        sym.isPackageObject // package objects can be visited before we get around to index them
        ||
        sym.owner != lastSymbol.owner &&
          (sym.owner.derivesFrom(lastSymbol.owner)
           ||
           selfTypeOf(sym).derivesFrom(lastSymbol.owner)
           ||
           selfTypeOf(lastSymbol).derivesFrom(sym.owner)
          )
        ||
        sym == defn.AnyClass.primaryConstructor,
        s"""data race? overwriting $lastSymbol with $sym in type $this,
           |last sym id = ${lastSymbol.id}, new sym id = ${sym.id},
           |last owner = ${lastSymbol.owner}, new owner = ${sym.owner},
           |period = ${ctx.phase} at run ${ctx.runId}""")
    }

    /** A reference with the initial symbol in `symd` has an info that
     *  might depend on the given prefix.
     */
    private def infoDependsOnPrefix(symd: SymDenotation, prefix: Type)(implicit ctx: Context): Boolean =
      symd.maybeOwner.membersNeedAsSeenFrom(prefix) && !symd.is(NonMember)

    /** Is this a reference to a class or object member? */
    def isMemberRef(implicit ctx: Context): Boolean = designator match {
      case sym: Symbol => infoDependsOnPrefix(sym, prefix)
      case _ => true
    }

    /** (1) Reduce a type-ref `W # X` or `W { ... } # U`, where `W` is a wildcard type
     *  to an (unbounded) wildcard type.
     *
     *  (2) Reduce a type-ref `T { X = U; ... } # X`  to   `U`
     *  provided `U` does not refer with a RecThis to the
     *  refinement type `T { X = U; ... }`
     */
    def reduceProjection(implicit ctx: Context): Type =
      if (isType) {
        val reduced = prefix.lookupRefined(name)
        if (reduced.exists) reduced else this
      }
      else this

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
      ctx.base.underlyingRecursions += 1
      if (ctx.base.underlyingRecursions < Config.LogPendingUnderlyingThreshold)
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
      ctx.base.underlyingRecursions -= 1
    }

    /** The argument corresponding to class type parameter `tparam` as seen from
     *  prefix `pre`. Can produce a TypeBounds type in case prefix is an & or | type
     *  and parameter is non-variant.
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
                case _: TypeBounds => TypeRef(pre, tparam)
                case arg => arg
              }
            tparams = tparams.tail
            args = args.tail
            idx += 1
          }
          NoType
        case base: AndOrType =>
          var tp1 = argForParam(base.tp1)
          var tp2 = argForParam(base.tp2)
          val variance = tparam.paramVariance
          if (isBounds(tp1) || isBounds(tp2) || variance == 0) {
            // compute argument as a type bounds instead of a point type
            tp1 = tp1.bounds
            tp2 = tp2.bounds
          }
          if (base.isAnd == variance >= 0) tp1 & tp2 else tp1 | tp2
        case _ =>
          if (pre.termSymbol is Package) argForParam(pre.select(nme.PACKAGE))
          else if (pre.isBottomType) pre
          else NoType
      }
    }

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
      else {
        if (isType) {
          val res =
            if (currentSymbol.is(ClassTypeParam)) argForParam(prefix)
            else prefix.lookupRefined(name)
          if (res.exists) return res
          if (Config.splitProjections)
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
            }
        }
        if (prefix.isInstanceOf[WildcardType]) WildcardType
        else withPrefix(prefix)
      }

    /** A reference like this one, but with the given symbol, if it exists */
    final def withSym(sym: Symbol)(implicit ctx: Context): ThisType =
      if ((designator ne sym) && sym.exists) NamedType(prefix, sym).asInstanceOf[ThisType]
      else this

    /** A reference like this one, but with the given denotation, if it exists.
     *  Returns a new named type with the denotation's symbol if that symbol exists, and
     *  one of the following alternatives applies:
     *   1. The current designator is a symbol and the symbols differ, or
     *   2. The current designator is a name and the new symbolic named type
     *      does not have a currently known denotation.
     *   3. The current designator is a name and the new symbolic named type
     *      has the same info as the current info
     *  Otherwise the current denotation is overwritten with the given one.
     *
     *  Note: (2) and (3) are a "lock in mechanism" where a reference with a name as
     *  designator can turn into a symbolic reference.
     *
     *  Note: This is a subtle dance to keep the balance between going to symbolic
     *  references as much as we can (since otherwise we'd risk getting cycles)
     *  and to still not lose any type info in the denotation (since symbolic
     *  references often recompute their info directly from the symbol's info).
     *  A test case is neg/opaque-self-encoding.scala.
     */
    final def withDenot(denot: Denotation)(implicit ctx: Context): ThisType =
      if (denot.exists) {
        val adapted = withSym(denot.symbol)
        val result =
          if (adapted.eq(this)
              || designator.isInstanceOf[Symbol]
              || !adapted.denotationIsCurrent
              || adapted.info.eq(denot.info))
            adapted
          else this
        result.setDenot(denot)
        result.asInstanceOf[ThisType]
      }
      else // don't assign NoDenotation, we might need to recover later. Test case is pos/avoid.scala.
        this

    /** A reference like this one, but with the given prefix. */
    final def withPrefix(prefix: Type)(implicit ctx: Context): NamedType = {
      def reload(): NamedType = {
        val allowPrivate = !lastSymbol.exists || lastSymbol.is(Private) && prefix.classSymbol == this.prefix.classSymbol
        var d = memberDenot(prefix, name, allowPrivate)
        if (d.isOverloaded && lastSymbol.exists)
          d = disambiguate(d,
                if (lastSymbol.signature == Signature.NotAMethod) Signature.NotAMethod
                else lastSymbol.asSeenFrom(prefix).signature)
        NamedType(prefix, name, d)
      }
      if (prefix eq this.prefix) this
      else if (lastDenotation == null) NamedType(prefix, designator)
      else designator match {
        case sym: Symbol =>
          if (infoDependsOnPrefix(sym, prefix) && !prefix.isArgPrefixOf(sym)) {
            val candidate = reload()
            val falseOverride = sym.isClass && candidate.symbol.exists && candidate.symbol != symbol
              // A false override happens if we rebind an inner class to another type with the same name
              // in an outer subclass. This is wrong, since classes do not override. We need to
              // return a type with the existing class info as seen from the new prefix instead.
            if (falseOverride) NamedType(prefix, sym.name, denot.asSeenFrom(prefix))
            else candidate
          }
          else NamedType(prefix, sym)
        case name: Name => reload()
      }
    }

    override def equals(that: Any): Boolean = equals(that, null)

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: NamedType =>
        designator.equals(that.designator) &&
        prefix.equals(that.prefix, bs)
      case _ =>
        false
    }

    override def computeHash(bs: Binders): Int = doHash(bs, designator, prefix)

    override def stableHash: Boolean = {
      if (myStableHash == 0) myStableHash = if (prefix.stableHash) 1 else -1
      myStableHash > 0
    }

    override def eql(that: Type): Boolean = this eq that // safe because named types are hash-consed separately
  }

  /** A reference to an implicit definition. This can be either a TermRef or a
   *  Implicits.RenamedImplicitRef.
   */
  trait ImplicitRef {
    def implicitName(implicit ctx: Context): TermName
    def underlyingRef: TermRef
  }

  /** The singleton type for path prefix#myDesignator.
    */
  abstract case class TermRef(override val prefix: Type,
                              private var myDesignator: Designator)
    extends NamedType with SingletonType with ImplicitRef {

    type ThisType = TermRef
    type ThisName = TermName

    override def designator: Designator = myDesignator
    override protected def designator_=(d: Designator): Unit = myDesignator = d

    //assert(name.toString != "<local Coder>")
    override def underlying(implicit ctx: Context): Type = {
      val d = denot
      if (d.isOverloaded) NoType else d.info
    }

    override def isOverloaded(implicit ctx: Context): Boolean = denot.isOverloaded

    def alternatives(implicit ctx: Context): List[TermRef] =
      denot.alternatives.map(withDenot(_))

    def altsWith(p: Symbol => Boolean)(implicit ctx: Context): List[TermRef] =
      denot.altsWith(p).map(withDenot(_))

    def implicitName(implicit ctx: Context): TermName = name
    def underlyingRef: TermRef = this
  }

  abstract case class TypeRef(override val prefix: Type,
                              private var myDesignator: Designator)
    extends NamedType {

    type ThisType = TypeRef
    type ThisName = TypeName

    override def designator: Designator = myDesignator
    override protected def designator_=(d: Designator): Unit = myDesignator = d

    override def underlying(implicit ctx: Context): Type = info

    override def translucentSuperType(implicit ctx: Context) = info match {
      case TypeAlias(aliased) => aliased
      case TypeBounds(_, hi) =>
        if (symbol.isOpaqueAlias)
          symbol.opaqueAlias.asSeenFrom(prefix, symbol.owner).orElse(hi) // orElse can happen for malformed input
        else hi
      case _ => underlying
    }

    /** Hook that can be called from creation methods in TermRef and TypeRef */
    def validated(implicit ctx: Context): this.type = {
      this
    }
  }

  final class CachedTermRef(prefix: Type, designator: Designator, hc: Int) extends TermRef(prefix, designator) {
    assert((prefix ne NoPrefix) || designator.isInstanceOf[Symbol])
    myHash = hc
  }

  final class CachedTypeRef(prefix: Type, designator: Designator, hc: Int) extends TypeRef(prefix, designator) {
    assert((prefix ne NoPrefix) || designator.isInstanceOf[Symbol])
    myHash = hc
  }

  /** Assert current phase does not have erasure semantics */
  private def assertUnerased()(implicit ctx: Context) =
    if (Config.checkUnerased) assert(!ctx.phase.erasedTypes)

  /** The designator to be used for a named type creation with given prefix, name, and denotation.
   *  This is the denotation's symbol, if it exists and the prefix is not the this type
   *  of the class owning the symbol. The reason for the latter qualification is that
   *  when re-computing the denotation of a `this.<symbol>` reference we read the
   *  type directly off the symbol. But the given denotation might contain a more precise
   *  type than what can be computed from the symbol's info. We have to create in this case
   *  a reference with a name as designator so that the denotation will be correctly updated in
   *  the future. See also NamedType#withDenot. Test case is neg/opaque-self-encoding.scala.
   */
  private def designatorFor(prefix: Type, name: Name, denot: Denotation)(implicit ctx: Context): Designator = {
    val sym = denot.symbol
    if (sym.exists && (prefix.eq(NoPrefix) || prefix.ne(sym.owner.thisType)))
      sym
    else
      name
  }

  object NamedType {
    def isType(desig: Designator)(implicit ctx: Context): Boolean = desig match {
      case sym: Symbol => sym.isType
      case name: Name => name.isTypeName
    }
    def apply(prefix: Type, designator: Designator)(implicit ctx: Context): NamedType =
      if (isType(designator)) TypeRef.apply(prefix, designator)
      else TermRef.apply(prefix, designator)
    def apply(prefix: Type, designator: Name, denot: Denotation)(implicit ctx: Context): NamedType =
      if (designator.isTermName) TermRef.apply(prefix, designator.asTermName, denot)
      else TypeRef.apply(prefix, designator.asTypeName, denot)
  }

  object TermRef {

    /** Create a term ref with given designator */
    def apply(prefix: Type, desig: Designator)(implicit ctx: Context): TermRef =
      ctx.uniqueNamedTypes.enterIfNew(prefix, desig, isTerm = true).asInstanceOf[TermRef]

    /** Create a term ref with given initial denotation. The name of the reference is taken
     *  from the denotation's symbol if the latter exists, or else it is the given name.
     */
    def apply(prefix: Type, name: TermName, denot: Denotation)(implicit ctx: Context): TermRef =
      apply(prefix, designatorFor(prefix, name, denot)).withDenot(denot)
  }

  object TypeRef {

    /** Create a type ref with given prefix and name */
    def apply(prefix: Type, desig: Designator)(implicit ctx: Context): TypeRef =
      ctx.uniqueNamedTypes.enterIfNew(prefix, desig, isTerm = false).asInstanceOf[TypeRef]

    /** Create a type ref with given initial denotation. The name of the reference is taken
     *  from the denotation's symbol if the latter exists, or else it is the given name.
     */
    def apply(prefix: Type, name: TypeName, denot: Denotation)(implicit ctx: Context): TypeRef =
      apply(prefix, designatorFor(prefix, name, denot)).withDenot(denot)
  }

  // --- Other SingletonTypes: ThisType/SuperType/ConstantType ---------------------------

  /** The type cls.this
   *  @param tref    A type ref which indicates the class `cls`.
   *  Note: we do not pass a class symbol directly, because symbols
   *  do not survive runs whereas typerefs do.
   */
  abstract case class ThisType(tref: TypeRef) extends CachedProxyType with SingletonType {
    def cls(implicit ctx: Context): ClassSymbol = tref.stableInRunSymbol match {
      case cls: ClassSymbol => cls
      case _ if ctx.mode.is(Mode.Interactive) => defn.AnyClass // was observed to happen in IDE mode
    }

    override def underlying(implicit ctx: Context): Type =
      if (ctx.erasedTypes) tref
      else cls.info match {
        case cinfo: ClassInfo => cinfo.selfType
        case _: ErrorType | NoType if ctx.mode.is(Mode.Interactive) => cls.info
          // can happen in IDE if `cls` is stale
      }

    override def computeHash(bs: Binders): Int = doHash(bs, tref)

    override def eql(that: Type): Boolean = that match {
      case that: ThisType => tref.eq(that.tref)
      case _ => false
    }
  }

  final class CachedThisType(tref: TypeRef) extends ThisType(tref)

  object ThisType {
    /** Normally one should use ClassSymbol#thisType instead */
    def raw(tref: TypeRef)(implicit ctx: Context): CachedThisType =
      unique(new CachedThisType(tref))
  }

  /** The type of a super reference cls.super where
   *  `thistpe` is cls.this and `supertpe` is the type of the value referenced
   *  by `super`.
   */
  abstract case class SuperType(thistpe: Type, supertpe: Type) extends CachedProxyType with SingletonType {
    override def underlying(implicit ctx: Context): Type = supertpe
    override def superType(implicit ctx: Context): Type =
      thistpe.baseType(supertpe.typeSymbol)
    def derivedSuperType(thistpe: Type, supertpe: Type)(implicit ctx: Context): Type =
      if ((thistpe eq this.thistpe) && (supertpe eq this.supertpe)) this
      else SuperType(thistpe, supertpe)

    override def computeHash(bs: Binders): Int = doHash(bs, thistpe, supertpe)

    override def eql(that: Type): Boolean = that match {
      case that: SuperType => thistpe.eq(that.thistpe) && supertpe.eq(that.supertpe)
      case _ => false
    }
  }

  final class CachedSuperType(thistpe: Type, supertpe: Type) extends SuperType(thistpe, supertpe)

  object SuperType {
    def apply(thistpe: Type, supertpe: Type)(implicit ctx: Context): Type = {
      assert(thistpe != NoPrefix)
      unique(new CachedSuperType(thistpe, supertpe))
    }
  }

  /** A constant type with single `value`. */
  abstract case class ConstantType(value: Constant) extends CachedProxyType with SingletonType {
    override def underlying(implicit ctx: Context): Type = value.tpe

    override def computeHash(bs: Binders): Int = doHash(value)
  }

  final class CachedConstantType(value: Constant) extends ConstantType(value)

  object ConstantType {
    def apply(value: Constant)(implicit ctx: Context): ConstantType = {
      assertUnerased()
      unique(new CachedConstantType(value))
    }
  }

  case class LazyRef(private var refFn: Context => Type) extends UncachedProxyType with ValueType {
    private[this] var myRef: Type = null
    private[this] var computed = false
    def ref(implicit ctx: Context): Type = {
      if (computed) {
        if (myRef == null) {
          // if errors were reported previously handle this by throwing a CyclicReference
          // instead of crashing immediately. A test case is neg/i6057.scala.
          assert(ctx.reporter.errorsReported)
          CyclicReference(NoDenotation)
        }
      }
      else {
        computed = true
        myRef = refFn(ctx)
        refFn = null
      }
      myRef
    }
    def evaluating: Boolean = computed && myRef == null
    def completed: Boolean = myRef != null
    override def underlying(implicit ctx: Context): Type = ref
    override def toString: String = s"LazyRef(${if (computed) myRef else "..."})"
    override def equals(other: Any): Boolean = this.eq(other.asInstanceOf[AnyRef])
    override def hashCode: Int = System.identityHashCode(this)
  }

  // --- Refined Type and RecType ------------------------------------------------

  abstract class RefinedOrRecType extends CachedProxyType with ValueType {
    def parent: Type
  }

  /** A refined type parent { refinement }
   *  @param parent      The type being refined
   *  @param refinedName The name of the refinement declaration
   *  @param refinedInfo The info of the refinement declaration
   */
  abstract case class RefinedType(parent: Type, refinedName: Name, refinedInfo: Type) extends RefinedOrRecType {

    if (refinedName.isTermName) assert(refinedInfo.isInstanceOf[TermType])
    else assert(refinedInfo.isInstanceOf[TypeType], this)
    assert(!refinedName.is(NameKinds.ExpandedName), this)

    override def underlying(implicit ctx: Context): Type = parent

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

    override def computeHash(bs: Binders): Int = doHash(bs, refinedName, refinedInfo, parent)
    override def stableHash: Boolean = refinedInfo.stableHash && parent.stableHash

    override def eql(that: Type): Boolean = that match {
      case that: RefinedType =>
        refinedName.eq(that.refinedName) &&
        refinedInfo.eq(that.refinedInfo) &&
        parent.eq(that.parent)
      case _ => false
    }

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: RefinedType =>
        refinedName.eq(that.refinedName) &&
        refinedInfo.equals(that.refinedInfo, bs) &&
        parent.equals(that.parent, bs)
      case _ => false
    }
    // equals comes from case class; no matching override is needed
  }

  class CachedRefinedType(parent: Type, refinedName: Name, refinedInfo: Type)
  extends RefinedType(parent, refinedName, refinedInfo)

  object RefinedType {
    @tailrec def make(parent: Type, names: List[Name], infos: List[Type])(implicit ctx: Context): Type =
      if (names.isEmpty) parent
      else make(RefinedType(parent, names.head, infos.head), names.tail, infos.tail)

    def apply(parent: Type, name: Name, info: Type)(implicit ctx: Context): RefinedType = {
      assert(!ctx.erasedTypes)
      unique(new CachedRefinedType(parent, name, info)).checkInst
    }
  }

  /** A recursive type. Instances should be constructed via the companion object.
   *
   *  @param parentExp A function that, given a recursive type R, produces a type
   *                   that can refer to R via a `RecThis(R)` node. This is used to
   *                   "tie the knot".
   *
   *  For example, in
   *    class C { type T1; type T2 }
   *    type C2 = C { type T1; type T2 = T1  }
   *
   *  The type of `C2` is a recursive type `{(x) => C{T1; T2 = x.T1}}`, written as
   *
   *    RecType(
   *      RefinedType(
   *        RefinedType(
   *         TypeRef(...,class C),
   *         T1,
   *         TypeBounds(...)),
   *        T2,
   *        TypeBounds(
   *          TypeRef(RecThis(...),T1),
   *          TypeRef(RecThis(...),T1))))
   *
   *  Where `RecThis(...)` points back to the enclosing `RecType`.
   */
  class RecType(parentExp: RecType => Type) extends RefinedOrRecType with BindingType {

    // See discussion in findMember#goRec why these vars are needed
    private[Types] var opened: Boolean = false
    private[Types] var openedTwice: Boolean = false

    val parent: Type = parentExp(this)

    private[this] var myRecThis: RecThis = null

    def recThis: RecThis = {
      if (myRecThis == null) myRecThis = new RecThisImpl(this)
      myRecThis
    }

    override def underlying(implicit ctx: Context): Type = parent

    def derivedRecType(parent: Type)(implicit ctx: Context): RecType =
      if (parent eq this.parent) this
      else RecType(rt => parent.substRecThis(this, rt.recThis))

    def rebind(parent: Type)(implicit ctx: Context): Type =
      if (parent eq this.parent) this
      else RecType.closeOver(rt => parent.substRecThis(this, rt.recThis))

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

    override def computeHash(bs: Binders): Int = doHash(new Binders(this, bs), parent)

    override def stableHash: Boolean = false
      // this is a conservative observation. By construction RecTypes contain at least
      // one RecThis occurrence. Since `stableHash` does not keep track of enclosing
      // bound types, it will return "unstable" for this occurrence and this would propagate.

    // No definition of `eql` --> fall back on equals, which calls iso

    override def equals(that: Any): Boolean = equals(that, null)

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: RecType =>
        parent.equals(that.parent, new BinderPairs(this, that, bs))
      case _ => false
    }

    override def toString: String = s"RecType($parent | $hashCode)"

    private def checkInst(implicit ctx: Context): this.type = this // debug hook
  }

  object RecType {

    /** Create a RecType, normalizing its contents. This means:
     *
     *   1. Nested Rec types on the type's spine are merged with the outer one.
     *   2. Any refinement of the form `type T = z.T` on the spine of the type
     *      where `z` refers to the created rec-type is replaced by
     *      `type T`. This avoids infinite recursions later when we
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
            case TypeAlias(ref @ TypeRef(RecThis(`rt`), _)) if ref.name == rname => TypeBounds.empty
            case _ => rinfo
          }
          tp.derivedRefinedType(normalize(parent), rname, rinfo1)
        case tp =>
          tp
      }
      unique(rt.derivedRecType(normalize(rt.parent))).checkInst
    }

    /** Create a `RecType`, but only if the type generated by `parentExp` is indeed recursive. */
    def closeOver(parentExp: RecType => Type)(implicit ctx: Context): Type = {
      val rt = this(parentExp)
      if (rt.isReferredToBy(rt.parent)) rt else rt.parent
    }
  }

  // --- AndType/OrType ---------------------------------------------------------------

  abstract class AndOrType extends CachedGroundType with ValueType {
    def isAnd: Boolean
    def tp1: Type
    def tp2: Type

    def derivedAndOrType(tp1: Type, tp2: Type)(implicit ctx: Context) =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else if (isAnd) AndType.make(tp1, tp2, checkValid = true)
      else OrType.make(tp1, tp2)
  }

  abstract case class AndType(tp1: Type, tp2: Type) extends AndOrType {
    def isAnd: Boolean = true
    private[this] var myBaseClassesPeriod: Period = Nowhere
    private[this] var myBaseClasses: List[ClassSymbol] = _
    /** Base classes of are the merge of the operand base classes. */
    override final def baseClasses(implicit ctx: Context): List[ClassSymbol] = {
      if (myBaseClassesPeriod != ctx.period) {
        val bcs1 = tp1.baseClasses
        val bcs1set = BaseClassSet(bcs1)
        def recur(bcs2: List[ClassSymbol]): List[ClassSymbol] = bcs2 match {
          case bc2 :: bcs2rest =>
            if (bcs1set contains bc2)
              if (bc2.is(Trait)) recur(bcs2rest)
              else bcs1 // common class, therefore rest is the same in both sequences
            else bc2 :: recur(bcs2rest)
          case nil => bcs1
        }
        myBaseClasses = recur(tp2.baseClasses)
        myBaseClassesPeriod = ctx.period
      }
      myBaseClasses
    }

    def derivedAndType(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else AndType.make(tp1, tp2, checkValid = true)

    def derived_& (tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else tp1 & tp2

    override def computeHash(bs: Binders): Int = doHash(bs, tp1, tp2)

    override def eql(that: Type): Boolean = that match {
      case that: AndType => tp1.eq(that.tp1) && tp2.eq(that.tp2)
      case _ => false
    }
  }

  final class CachedAndType(tp1: Type, tp2: Type) extends AndType(tp1, tp2)

  object AndType {
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context): AndType = {
      assert(tp1.isValueTypeOrWildcard &&
             tp2.isValueTypeOrWildcard, i"$tp1 & $tp2 / " + s"$tp1 & $tp2")
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

  abstract case class OrType(tp1: Type, tp2: Type) extends AndOrType {
    def isAnd: Boolean = false
    private[this] var myBaseClassesPeriod: Period = Nowhere
    private[this] var myBaseClasses: List[ClassSymbol] = _
    /** Base classes of are the intersection of the operand base classes. */
    override final def baseClasses(implicit ctx: Context): List[ClassSymbol] = {
      if (myBaseClassesPeriod != ctx.period) {
        val bcs1 = tp1.baseClasses
        val bcs1set = BaseClassSet(bcs1)
        def recur(bcs2: List[ClassSymbol]): List[ClassSymbol] = bcs2 match {
          case bc2 :: bcs2rest =>
            if (bcs1set contains bc2)
              if (bc2.is(Trait)) bc2 :: recur(bcs2rest)
              else bcs2
            else recur(bcs2rest)
          case nil =>
            bcs2
        }
        myBaseClasses = recur(tp2.baseClasses)
        myBaseClassesPeriod = ctx.period
      }
      myBaseClasses
    }

    assert(tp1.isValueTypeOrWildcard &&
           tp2.isValueTypeOrWildcard, s"$tp1 $tp2")

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

    private[this] var atomsRunId: RunId = NoRunId
    private[this] var myAtoms: Set[Type] = _
    private[this] var myWidened: Type = _

    private def ensureAtomsComputed()(implicit ctx: Context): Unit =
      if (atomsRunId != ctx.runId) {
        val atoms1 = tp1.atoms
        val atoms2 = tp2.atoms
        myAtoms = if (atoms1.nonEmpty && atoms2.nonEmpty) atoms1 | atoms2 else Set.empty
        val tp1w = tp1.widenSingletons
        val tp2w = tp2.widenSingletons
        myWidened = if ((tp1 eq tp1w) && (tp2 eq tp2w)) this else tp1w | tp2w
        atomsRunId = ctx.runId
      }

    override def atoms(implicit ctx: Context): Set[Type] = {
      ensureAtomsComputed()
      myAtoms
    }

    override def widenSingletons(implicit ctx: Context): Type = {
      ensureAtomsComputed()
      myWidened
    }

    def derivedOrType(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else OrType.make(tp1, tp2)

    override def computeHash(bs: Binders): Int = doHash(bs, tp1, tp2)

    override def eql(that: Type): Boolean = that match {
      case that: OrType => tp1.eq(that.tp1) && tp2.eq(that.tp2)
      case _ => false
    }
  }

  final class CachedOrType(tp1: Type, tp2: Type) extends OrType(tp1, tp2)

  object OrType {
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context): OrType = {
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
  trait SignatureCachingType extends TermType {
    protected[this] var mySignature: Signature = _
    protected[this] var mySignatureRunId: Int = NoRunId

    protected def computeSignature(implicit ctx: Context): Signature

    final override def signature(implicit ctx: Context): Signature = {
      if (ctx.runId != mySignatureRunId) {
        mySignature = computeSignature
        if (!mySignature.isUnderDefined) mySignatureRunId = ctx.runId
      }
      mySignature
    }
  }

  trait MethodicType extends SignatureCachingType {
    protected def resultSignature(implicit ctx: Context): Signature = try resultType match {
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
  }

  /** A by-name parameter type of the form `=> T`, or the type of a method with no parameter list. */
  abstract case class ExprType(resType: Type)
  extends CachedProxyType with TermType with MethodicType {
    override def resultType(implicit ctx: Context): Type = resType
    override def underlying(implicit ctx: Context): Type = resType

    def computeSignature(implicit ctx: Context): Signature = resultSignature

    def derivedExprType(resType: Type)(implicit ctx: Context): ExprType =
      if (resType eq this.resType) this else ExprType(resType)

    override def computeHash(bs: Binders): Int = doHash(bs, resType)
    override def stableHash: Boolean = resType.stableHash

    override def eql(that: Type): Boolean = that match {
      case that: ExprType => resType.eq(that.resType)
      case _ => false
    }

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: ExprType => resType.equals(that.resType, bs)
      case _ => false
    }
    // equals comes from case class; no matching override is needed
  }

  final class CachedExprType(resultType: Type) extends ExprType(resultType)

  object ExprType {
    def apply(resultType: Type)(implicit ctx: Context): ExprType = {
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
  trait LambdaType extends BindingType with TermType { self =>
    type ThisName <: Name
    type PInfo <: Type
    type This <: LambdaType{type PInfo = self.PInfo}
    type ParamRefType <: ParamRef

    def paramNames: List[ThisName]
    def paramInfos: List[PInfo]
    def resType: Type
    protected def newParamRef(n: Int): ParamRefType

    override def resultType(implicit ctx: Context): Type = resType

    def isResultDependent(implicit ctx: Context): Boolean
    def isParamDependent(implicit ctx: Context): Boolean

    final def isTermLambda: Boolean = isInstanceOf[TermLambda]
    final def isTypeLambda: Boolean = isInstanceOf[TypeLambda]
    final def isHigherKinded: Boolean = isInstanceOf[TypeProxy]

    private[this] var myParamRefs: List[ParamRefType] = null

    def paramRefs: List[ParamRefType] = {
      if (myParamRefs == null) myParamRefs = paramNames.indices.toList.map(newParamRef)
      myParamRefs
    }

    /** Like `paramInfos` but substitute parameter references with the given arguments */
    final def instantiateParamInfos(argTypes: => List[Type])(implicit ctx: Context): List[Type] =
      if (isParamDependent) paramInfos.mapConserve(_.substParams(this, argTypes))
      else paramInfos

    /** Like `resultType` but substitute parameter references with the given arguments */
    final def instantiate(argTypes: => List[Type])(implicit ctx: Context): Type =
      if (isResultDependent) resultType.substParams(this, argTypes)
      else resultType

    def companion: LambdaTypeCompanion[ThisName, PInfo, This]

    /** The type `[tparams := paramRefs] tp`, where `tparams` can be
     *  either a list of type parameter symbols or a list of lambda parameters
     */
    def integrate(tparams: List[ParamInfo], tp: Type)(implicit ctx: Context): Type =
      (tparams: @unchecked) match {
        case LambdaParam(lam, _) :: _ => tp.subst(lam, this)
        case params: List[Symbol @unchecked] => tp.subst(params, paramRefs)
      }

    final def derivedLambdaType(paramNames: List[ThisName] = this.paramNames,
                          paramInfos: List[PInfo] = this.paramInfos,
                          resType: Type = this.resType)(implicit ctx: Context): LambdaType =
      if ((paramNames eq this.paramNames) && (paramInfos eq this.paramInfos) && (resType eq this.resType)) this
      else newLikeThis(paramNames, paramInfos, resType)

    final def newLikeThis(paramNames: List[ThisName], paramInfos: List[PInfo], resType: Type)(implicit ctx: Context): This =
      companion(paramNames)(
          x => paramInfos.mapConserve(_.subst(this, x).asInstanceOf[PInfo]),
          x => resType.subst(this, x))

    protected def prefixString: String
    final override def toString: String = s"$prefixString($paramNames, $paramInfos, $resType)"
  }

  abstract class HKLambda extends CachedProxyType with LambdaType {
    final override def underlying(implicit ctx: Context): Type = resType

    override def computeHash(bs: Binders): Int =
      doHash(new Binders(this, bs), paramNames, resType, paramInfos)

    override def stableHash: Boolean = resType.stableHash && paramInfos.stableHash

    final override def equals(that: Any): Boolean = equals(that, null)

    // No definition of `eql` --> fall back on equals, which calls iso

    final override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: HKLambda =>
        paramNames.eqElements(that.paramNames) &&
        companion.eq(that.companion) && {
          val bs1 = new BinderPairs(this, that, bs)
          paramInfos.equalElements(that.paramInfos, bs1) &&
          resType.equals(that.resType, bs1)
        }
      case _ =>
        false
    }
  }

  abstract class MethodOrPoly extends UncachedGroundType with LambdaType with MethodicType {
    final override def hashCode: Int = System.identityHashCode(this)

    final override def equals(that: Any): Boolean = equals(that, null)

    // No definition of `eql` --> fall back on equals, which is `eq`

    final override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: MethodOrPoly =>
        paramNames.eqElements(that.paramNames) &&
        companion.eq(that.companion) && {
          val bs1 = new BinderPairs(this, that, bs)
          paramInfos.equalElements(that.paramInfos, bs1) &&
          resType.equals(that.resType, bs1)
        }
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
            case tp @ TypeRef(pre, _) =>
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

    private[this] var myDependencyStatus: DependencyStatus = Unknown
    private[this] var myParamDependencyStatus: DependencyStatus = Unknown

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
    def isResultDependent(implicit ctx: Context): Boolean = dependencyStatus == TrueDeps

    /** Does one of the parameter types contain references to earlier parameters
     *  of this method type which cannot be eliminated by de-aliasing?
     */
    def isParamDependent(implicit ctx: Context): Boolean = paramDependencyStatus == TrueDeps

    def newParamRef(n: Int): TermParamRef = new TermParamRefImpl(this, n)

    /** The least supertype of `resultType` that does not contain parameter dependencies */
    def nonDependentResultApprox(implicit ctx: Context): Type =
      if (isResultDependent) {
        val dropDependencies = new ApproximatingTypeMap {
          def apply(tp: Type) = tp match {
            case tp @ TermParamRef(thisLambdaType, _) =>
              range(defn.NothingType, atVariance(1)(apply(tp.underlying)))
            case _ => mapOver(tp)
          }
        }
        dropDependencies(resultType)
      }
      else resultType
  }

  abstract case class MethodType(paramNames: List[TermName])(
      paramInfosExp: MethodType => List[Type],
      resultTypeExp: MethodType => Type)
    extends MethodOrPoly with TermLambda with NarrowCached { thisMethodType =>

    type This = MethodType

    val paramInfos: List[Type] = paramInfosExp(this)
    val resType: Type = resultTypeExp(this)
    assert(resType.exists)

    def companion: MethodTypeCompanion

    final override def isJavaMethod: Boolean = companion eq JavaMethodType
    final override def isImplicitMethod: Boolean =
      companion.eq(ImplicitMethodType) ||
      companion.eq(ErasedImplicitMethodType) ||
      isContextualMethod
    final override def isErasedMethod: Boolean =
      companion.eq(ErasedMethodType) ||
      companion.eq(ErasedImplicitMethodType) ||
      companion.eq(ErasedContextualMethodType)
    final override def isContextualMethod: Boolean =
      companion.eq(ContextualMethodType) ||
      companion.eq(ErasedContextualMethodType)


    def computeSignature(implicit ctx: Context): Signature = {
      val params = if (isErasedMethod) Nil else paramInfos
      resultSignature.prepend(params, isJavaMethod)
    }

    protected def prefixString: String = companion.prefixString
  }

  final class CachedMethodType(paramNames: List[TermName])(paramInfosExp: MethodType => List[Type], resultTypeExp: MethodType => Type, val companion: MethodTypeCompanion)
    extends MethodType(paramNames)(paramInfosExp, resultTypeExp)

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
    def syntheticParamName(n: Int): TermName = nme.syntheticParamName(n)
  }

  abstract class TypeLambdaCompanion[LT <: TypeLambda]
  extends LambdaTypeCompanion[TypeName, TypeBounds, LT] {
    def toPInfo(tp: Type)(implicit ctx: Context): TypeBounds = (tp: @unchecked) match {
      case tp: TypeBounds => tp
      case tp: ErrorType => TypeAlias(tp)
    }
    def syntheticParamName(n: Int): TypeName = tpnme.syntheticTypeParamName(n)
  }

  abstract class MethodTypeCompanion(val prefixString: String) extends TermLambdaCompanion[MethodType] { self =>

    /** Produce method type from parameter symbols, with special mappings for repeated
     *  and inline parameters:
     *   - replace @repeated annotations on Seq or Array types by <repeated> types
     *   - add @inlineParam to inline call-by-value parameters
     */
    def fromSymbols(params: List[Symbol], resultType: Type)(implicit ctx: Context): MethodType = {
      def translateInline(tp: Type): Type = tp match {
        case _: ExprType => tp
        case _ => AnnotatedType(tp, Annotation(defn.InlineParamAnnot))
      }
      def paramInfo(param: Symbol) = {
        val paramType = param.info.annotatedToRepeated
        if (param.is(Inline)) translateInline(paramType) else paramType
      }

      apply(params.map(_.name.asTermName))(
         tl => params.map(p => tl.integrate(params, paramInfo(p))),
         tl => tl.integrate(params, resultType))
    }

    final def apply(paramNames: List[TermName])(paramInfosExp: MethodType => List[Type], resultTypeExp: MethodType => Type)(implicit ctx: Context): MethodType =
      checkValid(unique(new CachedMethodType(paramNames)(paramInfosExp, resultTypeExp, self)))

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

  object MethodType extends MethodTypeCompanion("MethodType") {
    def companion(isJava: Boolean = false, isContextual: Boolean = false, isImplicit: Boolean = false, isErased: Boolean = false): MethodTypeCompanion = {
      if (isJava) {
        assert(!isImplicit)
        assert(!isErased)
        assert(!isContextual)
        JavaMethodType
      }
      else if (isContextual)
        if (isErased) ErasedContextualMethodType else ContextualMethodType
      else if (isImplicit)
        if (isErased) ErasedImplicitMethodType else ImplicitMethodType
      else
        if (isErased) ErasedMethodType else MethodType
    }
  }
  object JavaMethodType extends MethodTypeCompanion("JavaMethodType")
  object ErasedMethodType extends MethodTypeCompanion("ErasedMethodType")
  object ContextualMethodType extends MethodTypeCompanion("ContextualMethodType")
  object ErasedContextualMethodType extends MethodTypeCompanion("ErasedContextualMethodType")
  object ImplicitMethodType extends MethodTypeCompanion("ImplicitMethodType")
  object ErasedImplicitMethodType extends MethodTypeCompanion("ErasedImplicitMethodType")

  /** A ternary extractor for MethodType */
  object MethodTpe {
    def unapply(mt: MethodType)(implicit ctx: Context): Some[(List[TermName], List[Type], Type)] =
      Some((mt.paramNames, mt.paramInfos, mt.resultType))
  }

  trait TypeLambda extends LambdaType {
    type ThisName = TypeName
    type PInfo = TypeBounds
    type This <: TypeLambda
    type ParamRefType = TypeParamRef

    def isResultDependent(implicit ctx: Context): Boolean = true
    def isParamDependent(implicit ctx: Context): Boolean = true

    def newParamRef(n: Int): TypeParamRef = new TypeParamRefImpl(this, n)

    @threadUnsafe lazy val typeParams: List[LambdaParam] =
      paramNames.indices.toList.map(new LambdaParam(this, _))

    def derivedLambdaAbstraction(paramNames: List[TypeName], paramInfos: List[TypeBounds], resType: Type)(implicit ctx: Context): Type =
      resType match {
        case resType: AliasingBounds =>
          resType.derivedAlias(newLikeThis(paramNames, paramInfos, resType.alias))
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
    def companion: HKTypeLambda.type = HKTypeLambda

    val paramInfos: List[TypeBounds] = paramInfosExp(this)
    val resType: Type = resultTypeExp(this)

    assert(resType.isInstanceOf[TermType], this)
    assert(paramNames.nonEmpty)

    protected def prefixString: String = "HKTypeLambda"
  }

  /** The type of a polymorphic method. It has the same form as HKTypeLambda,
   *  except it applies to terms and parameters do not have variances.
   */
  class PolyType(val paramNames: List[TypeName])(
      paramInfosExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type)
  extends MethodOrPoly with TypeLambda {

    type This = PolyType
    def companion: PolyType.type = PolyType

    val paramInfos: List[TypeBounds] = paramInfosExp(this)
    val resType: Type = resultTypeExp(this)

    assert(resType.isInstanceOf[TermType], this)
    assert(paramNames.nonEmpty)

    def computeSignature(implicit ctx: Context): Signature = resultSignature

    override def isContextualMethod = resType.isContextualMethod
    override def isImplicitMethod = resType.isImplicitMethod

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

    protected def prefixString: String = "PolyType"
  }

  object HKTypeLambda extends TypeLambdaCompanion[HKTypeLambda] {
    def apply(paramNames: List[TypeName])(
        paramInfosExp: HKTypeLambda => List[TypeBounds],
        resultTypeExp: HKTypeLambda => Type)(implicit ctx: Context): HKTypeLambda = {
      unique(new HKTypeLambda(paramNames)(paramInfosExp, resultTypeExp))
    }

    def unapply(tl: HKTypeLambda): Some[(List[LambdaParam], Type)] =
      Some((tl.typeParams, tl.resType))

    def any(n: Int)(implicit ctx: Context): HKTypeLambda =
      apply(syntheticParamNames(n))(
        pt => List.fill(n)(TypeBounds.empty), pt => defn.AnyType)

    override def paramName(param: ParamInfo.Of[TypeName])(implicit ctx: Context): TypeName =
      param.paramName.withVariance(param.paramVariance)

    /** Distributes Lambda inside type bounds. Examples:
     *
     *      type T[X] = U        becomes    type T = [X] -> U
     *      type T[X] <: U       becomes    type T >: Nothing <: ([X] -> U)
     *      type T[X] >: L <: U  becomes    type T >: ([X] -> L) <: ([X] -> U)
     */
    override def fromParams[PI <: ParamInfo.Of[TypeName]](params: List[PI], resultType: Type)(implicit ctx: Context): Type = {
      def expand(tp: Type) = super.fromParams(params, tp)
      resultType match {
        case rt: AliasingBounds =>
          rt.derivedAlias(expand(rt.alias))
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

    def any(n: Int)(implicit ctx: Context): PolyType =
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

  // ----- Type application: LambdaParam, AppliedType ---------------------

  /** The parameter of a type lambda */
  case class LambdaParam(tl: TypeLambda, n: Int) extends ParamInfo {
    type ThisName = TypeName
    def isTypeParam(implicit ctx: Context): Boolean = tl.paramNames.head.isTypeName
    def paramName(implicit ctx: Context): tl.ThisName = tl.paramNames(n)
    def paramInfo(implicit ctx: Context): tl.PInfo = tl.paramInfos(n)
    def paramInfoAsSeenFrom(pre: Type)(implicit ctx: Context): tl.PInfo = paramInfo
    def paramInfoOrCompleter(implicit ctx: Context): Type = paramInfo
    def paramVariance(implicit ctx: Context): Int = tl.paramNames(n).variance
    def paramRef(implicit ctx: Context): Type = tl.paramRefs(n)
  }

  /** A type application `C[T_1, ..., T_n]` */
  abstract case class AppliedType(tycon: Type, args: List[Type])
  extends CachedProxyType with ValueType {

    private[this] var validSuper: Period = Nowhere
    private[this] var cachedSuper: Type = _
    private[this] var myStableHash: Byte = 0

    private[this] var isGroundKnown: Boolean = false
    private[this] var isGroundCache: Boolean = _

    def isGround(acc: TypeAccumulator[Boolean])(implicit ctx: Context): Boolean = {
      if (!isGroundKnown) {
        isGroundCache = acc.foldOver(true, this)
        isGroundKnown = true
      }
      isGroundCache
    }

    override def underlying(implicit ctx: Context): Type = tycon

    override def superType(implicit ctx: Context): Type = {
      if (ctx.period != validSuper) {
        cachedSuper = tycon match {
          case tycon: HKTypeLambda => defn.AnyType
          case tycon: TypeRef if tycon.symbol.isClass => tycon
          case tycon: TypeProxy => tycon.superType.applyIfParameterized(args)
          case _ => defn.AnyType
        }
        validSuper = if (tycon.isProvisional) Nowhere else ctx.period
      }
      cachedSuper
    }

    override def translucentSuperType(implicit ctx: Context): Type = tycon match {
      case tycon: TypeRef if tycon.symbol.isOpaqueAlias =>
        tycon.translucentSuperType.applyIfParameterized(args)
      case _ =>
        superType
    }

    override def tryNormalize(implicit ctx: Context): Type = tycon match {
      case tycon: TypeRef =>
        def tryMatchAlias = tycon.info match {
          case MatchAlias(alias) =>
            trace(i"normalize $this", typr, show = true) {
              alias.applyIfParameterized(args).tryNormalize
            }
          case _ =>
            NoType
        }
        if (defn.isCompiletime_S(tycon.symbol) && args.length == 1) {
          trace(i"normalize S $this", typr, show = true) {
            args.head.normalized match {
              case ConstantType(Constant(n: Int)) => ConstantType(Constant(n + 1))
              case none => tryMatchAlias
            }
          }
        }
        else tryMatchAlias
      case _ =>
        NoType
    }

    def lowerBound(implicit ctx: Context): Type = tycon.stripTypeVar match {
      case tycon: TypeRef =>
        tycon.info match {
          case TypeBounds(lo, hi) =>
            if (lo eq hi) superType // optimization, can profit from caching in this case
            else lo.applyIfParameterized(args)
          case _ => NoType
        }
      case tycon: AppliedType =>
        tycon.lowerBound.applyIfParameterized(args)
      case _ =>
        NoType
    }

    def typeParams(implicit ctx: Context): List[ParamInfo] = {
      val tparams = tycon.typeParams
      if (tparams.isEmpty) HKTypeLambda.any(args.length).typeParams else tparams
    }

    def hasWildcardArg(implicit ctx: Context): Boolean = args.exists(isBounds)

    def derivedAppliedType(tycon: Type, args: List[Type])(implicit ctx: Context): Type =
      if ((tycon eq this.tycon) && (args eq this.args)) this
      else tycon.appliedTo(args)

    override def computeHash(bs: Binders): Int = doHash(bs, tycon, args)

    override def stableHash: Boolean = {
      if (myStableHash == 0) myStableHash = if (tycon.stableHash && args.stableHash) 1 else -1
      myStableHash > 0
    }

    override def eql(that: Type): Boolean = this `eq` that // safe because applied types are hash-consed separately

    final override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: AppliedType => tycon.equals(that.tycon, bs) && args.equalElements(that.args, bs)
      case _ => false
    }
    // equals comes from case class; no matching override is needed
  }

  final class CachedAppliedType(tycon: Type, args: List[Type], hc: Int) extends AppliedType(tycon, args) {
    myHash = hc
  }

  object AppliedType {
    def apply(tycon: Type, args: List[Type])(implicit ctx: Context): AppliedType = {
      assertUnerased()
      ctx.base.uniqueAppliedTypes.enterIfNew(tycon, args)
    }
  }

  // ----- BoundTypes: ParamRef, RecThis ----------------------------------------

  abstract class BoundType extends CachedProxyType with ValueType {
    type BT <: Type
    val binder: BT
    def copyBoundType(bt: BT): Type
    override def stableHash: Boolean = false
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

    override def computeHash(bs: Binders): Int = doHash(paramNum, binder.identityHash(bs))

    override def equals(that: Any): Boolean = equals(that, null)

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: ParamRef => paramNum == that.paramNum && binder.equalBinder(that.binder, bs)
      case _ => false
    }

    protected def kindString: String

    override def toString: String =
      try s"${kindString}ParamRef($paramName)"
      catch {
        case ex: IndexOutOfBoundsException => s"ParamRef(<bad index: $paramNum>)"
      }
  }

  /** Only created in `binder.paramRefs`. Use `binder.paramRefs(paramNum)` to
   *  refer to `TermParamRef(binder, paramNum)`.
   */
  abstract case class TermParamRef(binder: TermLambda, paramNum: Int) extends ParamRef with SingletonType {
    type BT = TermLambda
    def kindString: String = "Term"
    def copyBoundType(bt: BT): Type = bt.paramRefs(paramNum)
  }

  private final class TermParamRefImpl(binder: TermLambda, paramNum: Int) extends TermParamRef(binder, paramNum)

  /** Only created in `binder.paramRefs`. Use `binder.paramRefs(paramNum)` to
   *  refer to `TypeParamRef(binder, paramNum)`.
   */
  abstract case class TypeParamRef(binder: TypeLambda, paramNum: Int) extends ParamRef {
    type BT = TypeLambda
    def kindString: String = "Type"
    def copyBoundType(bt: BT): Type = bt.paramRefs(paramNum)

    /** Optimized version of occursIn, avoid quadratic blowup when solving
     *  constraints over large ground types.
     */
    override def occursIn(that: Type)(implicit ctx: Context): Boolean = !that.isGround && super.occursIn(that)

    /** Looking only at the structure of `bound`, is one of the following true?
     *     - fromBelow and param <:< bound
     *     - !fromBelow and param >:> bound
     */
    def occursIn(bound: Type, fromBelow: Boolean)(implicit ctx: Context): Boolean = bound.stripTypeVar match {
      case bound: ParamRef => bound == this
      case bound: AndType  => occursIn(bound.tp1, fromBelow) && occursIn(bound.tp2, fromBelow)
      case bound: OrType   => occursIn(bound.tp1, fromBelow) || occursIn(bound.tp2, fromBelow)
      case _ => false
    }
  }

  private final class TypeParamRefImpl(binder: TypeLambda, paramNum: Int) extends TypeParamRef(binder, paramNum)

  /** a self-reference to an enclosing recursive type. The only creation method is
   *  `binder.recThis`, returning `RecThis(binder)`.
   */
  abstract case class RecThis(binder: RecType) extends BoundType with SingletonType {
    type BT = RecType
    override def underlying(implicit ctx: Context): RecType = binder
    def copyBoundType(bt: BT): RecThis = bt.recThis

    // need to customize hashCode and equals to prevent infinite recursion
    // between RecTypes and RecRefs.
    override def computeHash(bs: Binders): Int = addDelta(binder.identityHash(bs), 41)

    override def equals(that: Any): Boolean = equals(that, null)

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: RecThis => binder.equalBinder(that.binder, bs)
      case _ => false
    }

    override def toString: String =
      try s"RecThis(${binder.hashCode})"
      catch {
        case ex: NullPointerException => s"RecThis(<under construction>)"
      }
  }

  private final class RecThisImpl(binder: RecType) extends RecThis(binder)

  // ----- Skolem types -----------------------------------------------

  /** A skolem type reference with underlying type `info`.
   *
   * For Dotty, a skolem type is a singleton type of some unknown value of type `info`.
   * Note that care is needed when creating them, since not all types need to be inhabited.
   * A skolem is equal to itself and no other type.
   */
  case class SkolemType(info: Type) extends UncachedProxyType with ValueType with SingletonType {
    override def underlying(implicit ctx: Context): Type = info
    def derivedSkolemType(info: Type)(implicit ctx: Context): SkolemType =
      if (info eq this.info) this else SkolemType(info)
    override def hashCode: Int = System.identityHashCode(this)
    override def equals(that: Any): Boolean = this.eq(that.asInstanceOf[AnyRef])

    def withName(name: Name): this.type = { myRepr = name; this }

    private[this] var myRepr: Name = null
    def repr(implicit ctx: Context): Name = {
      if (myRepr == null) myRepr = SkolemName.fresh()
      myRepr
    }

    override def toString: String = s"Skolem($hashCode)"
  }

  /** A skolem type used to wrap the type of the qualifier of a selection.
   *
   *  When typing a selection `e.f`, if `e` is unstable then we unconditionally
   *  skolemize it. We use a subclass of `SkolemType` for this so that
   *  [[TypeOps#asSeenFrom]] may treat it specially for optimization purposes,
   *  see its implementation for more details.
   */
  class QualSkolemType(info: Type) extends SkolemType(info) {
    override def derivedSkolemType(info: Type)(implicit ctx: Context): SkolemType =
      if (info eq this.info) this else QualSkolemType(info)
  }
  object QualSkolemType {
    def apply(info: Type): QualSkolemType = new QualSkolemType(info)
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
   *
   *  `owningTree` and `owner` are used to determine whether a type-variable can be instantiated
   *  at some given point. See `Inferencing#interpolateUndetVars`.
   */
  final class TypeVar(private var _origin: TypeParamRef, creatorState: TyperState) extends CachedProxyType with ValueType {

    def origin: TypeParamRef = _origin

    /** Set origin to new parameter. Called if we merge two conflicting constraints.
     *  See OrderingConstraint#merge, OrderingConstraint#rename
     */
    def setOrigin(p: TypeParamRef) = _origin = p

    /** The permanent instance type of the variable, or NoType is none is given yet */
    private[this] var myInst: Type = NoType

    private[core] def inst: Type = myInst
    private[core] def inst_=(tp: Type): Unit = {
      myInst = tp
      if (tp.exists && (owningState ne null)) {
        owningState.get.ownedVars -= this
        owningState = null // no longer needed; null out to avoid a memory leak
      }
    }

    /** The state owning the variable. This is at first `creatorState`, but it can
     *  be changed to an enclosing state on a commit.
     */
    private[core] var owningState: WeakReference[TyperState] =
      if (creatorState == null) null else new WeakReference(creatorState)

    /** The instance type of this variable, or NoType if the variable is currently
     *  uninstantiated
     */
    def instanceOpt(implicit ctx: Context): Type =
      if (inst.exists) inst else ctx.typeComparer.instType(this)

    /** Is the variable already instantiated? */
    def isInstantiated(implicit ctx: Context): Boolean = instanceOpt.exists

    /** Instantiate variable with given type */
    def instantiateWith(tp: Type)(implicit ctx: Context): Type = {
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

    /** For uninstantiated type variables: Is the lower bound different from Nothing? */
    def hasLowerBound(implicit ctx: Context): Boolean =
      !ctx.typerState.constraint.entry(origin).loBound.isBottomType

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
      if (inst.exists) inst else origin
    }

    override def computeHash(bs: Binders): Int = identityHash(bs)
    override def equals(that: Any): Boolean = this.eq(that.asInstanceOf[AnyRef])

    override def toString: String = {
      def instStr = if (inst.exists) s" -> $inst" else ""
      s"TypeVar($origin$instStr)"
    }
  }

  type TypeVars = SimpleIdentitySet[TypeVar]

  // ------ MatchType ---------------------------------------------------------------

  /**    scrutinee match { case_1 ... case_n }
   *
   *  where
   *
   *     case_i  =   [X1, ..., Xn] patternType => resultType
   *
   *  and `X_1,...X_n` are the type variables bound in `patternType`
   */
  abstract case class MatchType(bound: Type, scrutinee: Type, cases: List[Type]) extends CachedProxyType with ValueType {

    def derivedMatchType(bound: Type, scrutinee: Type, cases: List[Type])(implicit ctx: Context): MatchType =
      if (bound.eq(this.bound) && scrutinee.eq(this.scrutinee) && cases.eqElements(this.cases)) this
      else MatchType(bound, scrutinee, cases)

    def caseType(tp: Type)(implicit ctx: Context): Type = tp match {
      case tp: HKTypeLambda => caseType(tp.resType)
      case defn.MatchCase(_, body) => body
    }

    def alternatives(implicit ctx: Context): List[Type] = cases.map(caseType)
    def underlying(implicit ctx: Context): Type = bound

    private[this] var myReduced: Type = null
    private[this] var reductionContext: mutable.Map[Type, Type] = null

    override def tryNormalize(implicit ctx: Context): Type = reduced.normalized

    def reduced(implicit ctx: Context): Type = {
      val trackingCtx = ctx.fresh.setTypeComparerFn(new TrackingTypeComparer(_))
      val typeComparer = trackingCtx.typeComparer.asInstanceOf[TrackingTypeComparer]

      def contextInfo(tp: Type): Type = tp match {
        case tp: TypeParamRef =>
          val constraint = ctx.typerState.constraint
          if (constraint.entry(tp).exists) ctx.typeComparer.fullBounds(tp)
          else NoType
        case tp: TypeRef =>
          val bounds = ctx.gadt.fullBounds(tp.symbol)
          if (bounds == null) NoType else bounds
        case tp: TypeVar =>
          tp.underlying
      }

      def updateReductionContext(): Unit = {
        reductionContext = new mutable.HashMap
        for (tp <- typeComparer.footprint)
          reductionContext(tp) = contextInfo(tp)
        typr.println(i"footprint for $this $hashCode: ${typeComparer.footprint.toList.map(x => (x, contextInfo(x)))}%, %")
      }

      def isUpToDate: Boolean =
        reductionContext.keysIterator.forall { tp =>
          reductionContext(tp) `eq` contextInfo(tp)
        }

      record("MatchType.reduce called")
      if (!Config.cacheMatchReduced || myReduced == null || !isUpToDate) {
        record("MatchType.reduce computed")
        if (myReduced != null) record("MatchType.reduce cache miss")
        myReduced =
          trace(i"reduce match type $this $hashCode", typr, show = true) {
            try
              typeComparer.matchCases(scrutinee, cases)(trackingCtx)
            catch {
              case ex: Throwable =>
                handleRecursive("reduce type ", i"$scrutinee match ...", ex)
            }
          }
        updateReductionContext()
      }
      myReduced
    }

    override def computeHash(bs: Binders): Int = doHash(bs, scrutinee, bound :: cases)

    override def eql(that: Type): Boolean = that match {
      case that: MatchType =>
        bound.eq(that.bound) && scrutinee.eq(that.scrutinee) && cases.eqElements(that.cases)
      case _ => false
    }
  }

  class CachedMatchType(bound: Type, scrutinee: Type, cases: List[Type]) extends MatchType(bound, scrutinee, cases)

  object MatchType {
    def apply(bound: Type, scrutinee: Type, cases: List[Type])(implicit ctx: Context): MatchType =
      unique(new CachedMatchType(bound, scrutinee, cases))
  }

  // ------ ClassInfo, Type Bounds --------------------------------------------------

  type TypeOrSymbol = AnyRef /* should be: Type | Symbol */

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
      classParents: List[Type],
      decls: Scope,
      selfInfo: TypeOrSymbol) extends CachedGroundType with TypeType {

    private[this] var selfTypeCache: Type = null
    private[this] var appliedRefCache: Type = null

    /** The self type of a class is the conjunction of
     *   - the explicit self type if given (or the info of a given self symbol), and
     *   - the fully applied reference to the class itself.
     */
    def selfType(implicit ctx: Context): Type = {
      if (selfTypeCache == null)
        selfTypeCache = {
          val givenSelf = cls.givenSelfType
          if (!givenSelf.isValueType) appliedRef
          else if (cls is Module) givenSelf
          else if (ctx.erasedTypes) appliedRef
          else AndType(givenSelf, appliedRef)
        }
      selfTypeCache
    }

    def appliedRef(implicit ctx: Context): Type = {
      if (appliedRefCache == null)
        appliedRefCache =
          TypeRef(prefix, cls).appliedTo(cls.typeParams.map(_.typeRef))
      appliedRefCache
    }

    // cached because baseType needs parents
    private[this] var parentsCache: List[Type] = null

    override def parents(implicit ctx: Context): List[Type] = {
      if (parentsCache == null)
        parentsCache = classParents.mapConserve(_.asSeenFrom(prefix, cls.owner))
      parentsCache
    }

    def derivedClassInfo(prefix: Type)(implicit ctx: Context): ClassInfo =
      if (prefix eq this.prefix) this
      else ClassInfo(prefix, cls, classParents, decls, selfInfo)

    def derivedClassInfo(prefix: Type = this.prefix, classParents: List[Type] = this.classParents, decls: Scope = this.decls, selfInfo: TypeOrSymbol = this.selfInfo)(implicit ctx: Context): ClassInfo =
      if ((prefix eq this.prefix) && (classParents eq this.classParents) && (decls eq this.decls) && (selfInfo eq this.selfInfo)) this
      else ClassInfo(prefix, cls, classParents, decls, selfInfo)

    override def computeHash(bs: Binders): Int = doHash(bs, cls, prefix)
    override def stableHash: Boolean = prefix.stableHash && classParents.stableHash

    override def eql(that: Type): Boolean = that match {
      case that: ClassInfo =>
        prefix.eq(that.prefix) &&
        cls.eq(that.cls) &&
        classParents.eqElements(that.classParents) &&
        decls.eq(that.decls) &&
        selfInfo.eq(that.selfInfo)
      case _ => false
    }

    override def equals(that: Any): Boolean = equals(that, null)

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: ClassInfo =>
        prefix.equals(that.prefix, bs) &&
        cls.eq(that.cls) &&
        classParents.equalElements(that.classParents, bs) &&
        decls.eq(that.decls) &&
        selfInfo.eq(that.selfInfo)
      case _ => false
    }

    override def toString: String = s"ClassInfo($prefix, $cls, $classParents)"
  }

  class CachedClassInfo(prefix: Type, cls: ClassSymbol, classParents: List[Type], decls: Scope, selfInfo: TypeOrSymbol)
    extends ClassInfo(prefix, cls, classParents, decls, selfInfo)

  /** A class for temporary class infos where `parents` are not yet known */
  final class TempClassInfo(prefix: Type, cls: ClassSymbol, decls: Scope, selfInfo: TypeOrSymbol)
  extends CachedClassInfo(prefix, cls, Nil, decls, selfInfo) {

    /** Install classinfo with known parents in `denot` s */
    def finalize(denot: SymDenotation, parents: List[Type], selfInfo: TypeOrSymbol)(implicit ctx: Context): Unit =
      denot.info = ClassInfo(prefix, cls, parents, decls, selfInfo)

    override def derivedClassInfo(prefix: Type)(implicit ctx: Context): ClassInfo =
      if (prefix eq this.prefix) this
      else new TempClassInfo(prefix, cls, decls, selfInfo)

    override def toString: String = s"TempClassInfo($prefix, $cls)"
  }

  object ClassInfo {
    def apply(prefix: Type, cls: ClassSymbol, classParents: List[Type], decls: Scope, selfInfo: TypeOrSymbol = NoType)(implicit ctx: Context): ClassInfo =
      unique(new CachedClassInfo(prefix, cls, classParents, decls, selfInfo))
  }

  /** Type bounds >: lo <: hi */
  abstract case class TypeBounds(lo: Type, hi: Type) extends CachedProxyType with TypeType {

    assert(lo.isInstanceOf[TermType], lo)
    assert(hi.isInstanceOf[TermType], hi)

    override def underlying(implicit ctx: Context): Type = hi

    /** The non-alias type bounds type with given bounds */
    def derivedTypeBounds(lo: Type, hi: Type)(implicit ctx: Context): TypeBounds =
      if ((lo eq this.lo) && (hi eq this.hi)) this
      else TypeBounds(lo, hi)

    def contains(tp: Type)(implicit ctx: Context): Boolean = tp match {
      case tp: TypeBounds => lo <:< tp.lo && tp.hi <:< hi
      case tp: ClassInfo =>
        val cls = tp.cls
        // Note: Taking a normal typeRef does not work here. A normal ref might contain
        // also other information about the named type (e.g. bounds).
        contains(
          TypeRef(tp.prefix, cls).withDenot(new UniqueRefDenotation(cls, tp, cls.validFor)))
      case _ =>
        lo <:< tp && tp <:< hi
    }

    def & (that: TypeBounds)(implicit ctx: Context): TypeBounds =
      if ((this.lo frozen_<:< that.lo) && (that.hi frozen_<:< this.hi)) that
      else if ((that.lo frozen_<:< this.lo) && (this.hi frozen_<:< that.hi)) this
      else TypeBounds(this.lo | that.lo, this.hi & that.hi)

    def | (that: TypeBounds)(implicit ctx: Context): TypeBounds =
      if ((this.lo frozen_<:< that.lo) && (that.hi frozen_<:< this.hi)) this
      else if ((that.lo frozen_<:< this.lo) && (this.hi frozen_<:< that.hi)) that
      else TypeBounds(this.lo & that.lo, this.hi | that.hi)

    override def & (that: Type)(implicit ctx: Context): Type = that match {
      case that: TypeBounds => this & that
      case _ => super.& (that)
    }

    override def | (that: Type)(implicit ctx: Context): Type = that match {
      case that: TypeBounds => this | that
      case _ => super.| (that)
    }

    override def computeHash(bs: Binders): Int = doHash(bs, lo, hi)
    override def stableHash: Boolean = lo.stableHash && hi.stableHash

    override def equals(that: Any): Boolean = equals(that, null)

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: AliasingBounds => false
      case that: TypeBounds => lo.equals(that.lo, bs) && hi.equals(that.hi, bs)
      case _ => false
    }

    override def eql(that: Type): Boolean = that match {
      case that: AliasingBounds => false
      case that: TypeBounds => lo.eq(that.lo) && hi.eq(that.hi)
      case _ => false
    }
  }

  class RealTypeBounds(lo: Type, hi: Type) extends TypeBounds(lo, hi)

  /** Common supertype of `TypeAlias` and `MatchAlias` */
  abstract class AliasingBounds(val alias: Type) extends TypeBounds(alias, alias) {

    def derivedAlias(alias: Type)(implicit ctx: Context): AliasingBounds

    override def computeHash(bs: Binders): Int = doHash(bs, alias)
    override def stableHash: Boolean = alias.stableHash

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: AliasingBounds => this.isTypeAlias == that.isTypeAlias && alias.equals(that.alias, bs)
      case _ => false
    }
    // equals comes from case class; no matching override is needed

    override def eql(that: Type): Boolean = that match {
      case that: AliasingBounds => this.isTypeAlias == that.isTypeAlias && alias.eq(that.alias)
      case _ => false
    }
  }

  /**    = T
   */
  class TypeAlias(alias: Type) extends AliasingBounds(alias) {
    def derivedAlias(alias: Type)(implicit ctx: Context): AliasingBounds =
      if (alias eq this.alias) this else TypeAlias(alias)
  }

  /**    = T     where `T` is a `MatchType`
   *
   *  Match aliases are treated differently from type aliases. Their sides are mutually
   *  subtypes of each other but one side is not generally substitutable for the other.
   *  If we assumed full substitutivity, we would have to reject all recursive match
   *  aliases (or else take the jump and allow full recursive types).
   */
  class MatchAlias(alias: Type) extends AliasingBounds(alias) {
    def derivedAlias(alias: Type)(implicit ctx: Context): AliasingBounds =
      if (alias eq this.alias) this else MatchAlias(alias)
  }

  object TypeBounds {
    def apply(lo: Type, hi: Type)(implicit ctx: Context): TypeBounds =
      unique(new RealTypeBounds(lo, hi))
    def empty(implicit ctx: Context): TypeBounds = apply(defn.NothingType, defn.AnyType)
    def upper(hi: Type)(implicit ctx: Context): TypeBounds = apply(defn.NothingType, hi)
    def lower(lo: Type)(implicit ctx: Context): TypeBounds = apply(lo, defn.AnyType)
  }

  object TypeAlias {
    def apply(alias: Type)(implicit ctx: Context): TypeAlias = unique(new TypeAlias(alias))
    def unapply(tp: TypeAlias): Option[Type] = Some(tp.alias)
  }

  object MatchAlias {
    def apply(alias: Type)(implicit ctx: Context): MatchAlias = unique(new MatchAlias(alias))
    def unapply(tp: MatchAlias): Option[Type] = Some(tp.alias)
  }

  // ----- Annotated and Import types -----------------------------------------------

  /** An annotated type tpe @ annot */
  case class AnnotatedType(parent: Type, annot: Annotation) extends UncachedProxyType with ValueType {
    // todo: cache them? but this makes only sense if annotations and trees are also cached.

    override def underlying(implicit ctx: Context): Type = parent

    def derivedAnnotatedType(parent: Type, annot: Annotation): AnnotatedType =
      if ((parent eq this.parent) && (annot eq this.annot)) this
      else AnnotatedType(parent, annot)

    override def stripTypeVar(implicit ctx: Context): Type =
      derivedAnnotatedType(parent.stripTypeVar, annot)

    override def stripAnnots(implicit ctx: Context): Type = parent.stripAnnots

    private[this] var isRefiningKnown = false
    private[this] var isRefiningCache: Boolean = _

    def isRefining(implicit ctx: Context): Boolean = {
      if (!isRefiningKnown) {
        isRefiningCache = annot.symbol.derivesFrom(defn.RefiningAnnotationClass)
        isRefiningKnown = true
      }
      isRefiningCache
    }

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: AnnotatedType => parent.equals(that.parent, bs) && (annot `eq` that.annot)
      case _ => false
    }
    // equals comes from case class; no matching override is needed
  }

  object AnnotatedType {
    def make(underlying: Type, annots: List[Annotation]): Type =
      (underlying /: annots)(AnnotatedType(_, _))
  }

  // Special type objects and classes -----------------------------------------------------

  /** The type of an erased array */
  abstract case class JavaArrayType(elemType: Type) extends CachedGroundType with ValueType {
    def derivedJavaArrayType(elemtp: Type)(implicit ctx: Context): JavaArrayType =
      if (elemtp eq this.elemType) this else JavaArrayType(elemtp)

    override def computeHash(bs: Binders): Int = doHash(bs, elemType)
    override def stableHash: Boolean = elemType.stableHash

    override def eql(that: Type): Boolean = that match {
      case that: JavaArrayType => elemType.eq(that.elemType)
      case _ => false
    }
  }
  final class CachedJavaArrayType(elemType: Type) extends JavaArrayType(elemType)
  object JavaArrayType {
    def apply(elemType: Type)(implicit ctx: Context): JavaArrayType = unique(new CachedJavaArrayType(elemType))
  }

  /** The type of an import clause tree */
  case class ImportType(expr: Tree) extends UncachedGroundType

  /** Sentinel for "missing type" */
  @sharable case object NoType extends CachedGroundType {
    override def computeHash(bs: Binders): Int = hashSeed
  }

  /** Missing prefix */
  @sharable case object NoPrefix extends CachedGroundType {
    override def computeHash(bs: Binders): Int = hashSeed
  }

  /** A common superclass of `ErrorType` and `TryDynamicCallSite`. Instances of this
   *  class are at the same time subtypes and supertypes of every other type.
   */
  abstract class FlexType extends UncachedGroundType with ValueType

  abstract class ErrorType extends FlexType {
    def msg(implicit ctx: Context): Message
  }

  object ErrorType {
    def apply(msg: => Message)(implicit ctx: Context): ErrorType = {
      val et = new ErrorType {
        def msg(implicit ctx: Context): Message =
          ctx.base.errorTypeMsg.get(this) match {
            case Some(msgFun) => msgFun()
            case None => "error message from previous run no longer available"
          }
      }
      ctx.base.errorTypeMsg(et) = () => msg
      et
    }
  }

  object UnspecifiedErrorType extends ErrorType {
    override def msg(implicit ctx: Context): Message = "unspecified error"
  }

  /* Type used to track Select nodes that could not resolve a member and their qualifier is a scala.Dynamic. */
  object TryDynamicCallType extends FlexType

  /** Wildcard type, possibly with bounds */
  abstract case class WildcardType(optBounds: Type) extends CachedGroundType with TermType {
    def derivedWildcardType(optBounds: Type)(implicit ctx: Context): WildcardType =
      if (optBounds eq this.optBounds) this
      else if (!optBounds.exists) WildcardType
      else WildcardType(optBounds.asInstanceOf[TypeBounds])

    override def computeHash(bs: Binders): Int = doHash(bs, optBounds)
    override def stableHash: Boolean = optBounds.stableHash

    override def eql(that: Type): Boolean = that match {
      case that: WildcardType => optBounds.eq(that.optBounds)
      case _ => false
    }

    override def iso(that: Any, bs: BinderPairs): Boolean = that match {
      case that: WildcardType => optBounds.equals(that.optBounds, bs)
      case _ => false
    }
    // equals comes from case class; no matching override is needed
  }

  final class CachedWildcardType(optBounds: Type) extends WildcardType(optBounds)

  @sharable object WildcardType extends WildcardType(NoType) {
    def apply(bounds: TypeBounds)(implicit ctx: Context): WildcardType = unique(new CachedWildcardType(bounds))
  }

  /** An extractor for single abstract method types.
   *  A type is a SAM type if it is a reference to a class or trait, which
   *
   *   - has a single abstract method with a method type (ExprType
   *     and PolyType not allowed!) whose result type is not an implicit function type
   *   - can be instantiated without arguments or with just () as argument.
   *
   *  The pattern `SAMType(sam)` matches a SAM type, where `sam` is the
   *  type of the single abstract method.
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
      case _ =>
        NoType
    }
    def isInstantiatable(tp: Type)(implicit ctx: Context): Boolean = zeroParamClass(tp) match {
      case cinfo: ClassInfo =>
        val selfType = cinfo.selfType.asSeenFrom(tp, cinfo.cls)
        tp <:< selfType
      case _ =>
        false
    }
    def unapply(tp: Type)(implicit ctx: Context): Option[MethodType] =
      if (isInstantiatable(tp)) {
        val absMems = tp.abstractTermMembers
        // println(s"absMems: ${absMems map (_.show) mkString ", "}")
        if (absMems.size == 1)
          absMems.head.info match {
            case mt: MethodType if !mt.isParamDependent &&
                !defn.isImplicitFunctionType(mt.resultType) =>
              val cls = tp.classSymbol

              // Given a SAM type such as:
              //
              //     import java.util.function.Function
              //     Function[_ >: String, _ <: Int]
              //
              // the single abstract method will have type:
              //
              //     (x: Function[_ >: String, _ <: Int]#T): Function[_ >: String, _ <: Int]#R
              //
              // which is not implementable outside of the scope of Function.
              //
              // To avoid this kind of issue, we approximate references to
              // parameters of the SAM type by their bounds, this way in the
              // above example we get:
              //
              //    (x: String): Int
              val approxParams = new ApproximatingTypeMap {
                def apply(tp: Type): Type = tp match {
                  case tp: TypeRef if tp.symbol.is(ClassTypeParam) && tp.symbol.owner == cls =>
                    tp.info match {
                      case info: AliasingBounds =>
                        mapOver(info.alias)
                      case TypeBounds(lo, hi) =>
                        range(atVariance(-variance)(apply(lo)), apply(hi))
                       case _ =>
                        range(defn.NothingType, defn.AnyType) // should happen only in error cases
                    }
                  case _ =>
                    mapOver(tp)
                }
              }
              val approx = approxParams(mt).asInstanceOf[MethodType]
              Some(approx)
            case _ =>
              None
          }
        else if (tp isRef defn.PartialFunctionClass)
          // To maintain compatibility with 2.x, we treat PartialFunction specially,
          // pretending it is a SAM type. In the future it would be better to merge
          // Function and PartialFunction, have Function1 contain a isDefinedAt method
          //     def isDefinedAt(x: T) = true
          // and overwrite that method whenever the function body is a sequence of
          // case clauses.
          absMems.find(_.symbol.name == nme.apply).map(_.info.asInstanceOf[MethodType])
        else None
      }
      else None
  }

  // ----- TypeMaps --------------------------------------------------------------------

  /** Common base class of TypeMap and TypeAccumulator */
  abstract class VariantTraversal {
    protected[core] var variance: Int = 1

    @forceInline protected def atVariance[T](v: Int)(op: => T): T = {
      val saved = variance
      variance = v
      val res = op
      variance = saved
      res
    }
  }

  abstract class TypeMap(implicit protected val ctx: Context)
  extends VariantTraversal with (Type => Type) { thisMap =>

    protected def stopAtStatic: Boolean = true

    def apply(tp: Type): Type

    protected def derivedSelect(tp: NamedType, pre: Type): Type =
      tp.derivedSelect(pre)
    protected def derivedRefinedType(tp: RefinedType, parent: Type, info: Type): Type =
      tp.derivedRefinedType(parent, tp.refinedName, info)
    protected def derivedRecType(tp: RecType, parent: Type): Type =
      tp.rebind(parent)
    protected def derivedAlias(tp: AliasingBounds, alias: Type): Type =
      tp.derivedAlias(alias)
    protected def derivedTypeBounds(tp: TypeBounds, lo: Type, hi: Type): Type =
      tp.derivedTypeBounds(lo, hi)
    protected def derivedSuperType(tp: SuperType, thistp: Type, supertp: Type): Type =
      tp.derivedSuperType(thistp, supertp)
    protected def derivedAppliedType(tp: AppliedType, tycon: Type, args: List[Type]): Type =
      tp.derivedAppliedType(tycon, args)
    protected def derivedAndType(tp: AndType, tp1: Type, tp2: Type): Type =
      tp.derivedAndType(tp1, tp2)
    protected def derivedOrType(tp: OrType, tp1: Type, tp2: Type): Type =
      tp.derivedOrType(tp1, tp2)
    protected def derivedMatchType(tp: MatchType, bound: Type, scrutinee: Type, cases: List[Type]): Type =
      tp.derivedMatchType(bound, scrutinee, cases)
    protected def derivedAnnotatedType(tp: AnnotatedType, underlying: Type, annot: Annotation): Type =
      tp.derivedAnnotatedType(underlying, annot)
    protected def derivedWildcardType(tp: WildcardType, bounds: Type): Type =
      tp.derivedWildcardType(bounds)
    protected def derivedSkolemType(tp: SkolemType, info: Type): Type =
      tp.derivedSkolemType(info)
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
      record(s"mapOver ${getClass}")
      record("mapOver total")
      implicit val ctx = this.ctx
      tp match {
        case tp: NamedType =>
          if (stopAtStatic && tp.symbol.isStatic || (tp.prefix `eq` NoPrefix)) tp
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
          def mapArgs(args: List[Type], tparams: List[ParamInfo]): List[Type] = args match {
            case arg :: otherArgs =>
              val arg1 = arg match {
                case arg: TypeBounds => this(arg)
                case arg => atVariance(variance * tparams.head.paramVariance)(this(arg))
              }
              val otherArgs1 = mapArgs(otherArgs, tparams.tail)
              if ((arg1 eq arg) && (otherArgs1 eq otherArgs)) args
              else arg1 :: otherArgs1
            case nil =>
              nil
          }
          derivedAppliedType(tp, this(tp.tycon), mapArgs(tp.args, tp.typeParams))

        case tp: RefinedType =>
          derivedRefinedType(tp, this(tp.parent), this(tp.refinedInfo))

        case tp: AliasingBounds =>
          derivedAlias(tp, atVariance(0)(this(tp.alias)))

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

        case tp: ExprType =>
          derivedExprType(tp, this(tp.resultType))

        case tp: LambdaType =>
          def mapOverLambda = {
            val restpe = tp.resultType
            val saved = variance
            variance = if (defn.MatchCase.isInstance(restpe)) 0 else -variance
            val ptypes1 = tp.paramInfos.mapConserve(this).asInstanceOf[List[tp.PInfo]]
            variance = saved
            derivedLambdaType(tp)(ptypes1, this(restpe))
          }
          mapOverLambda

        case tp @ SuperType(thistp, supertp) =>
          derivedSuperType(tp, this(thistp), this(supertp))

        case tp: LazyRef =>
          LazyRef(_ => this(tp.ref))

        case tp: ClassInfo =>
          mapClassInfo(tp)

        case tp: AndType =>
          derivedAndType(tp, this(tp.tp1), this(tp.tp2))

        case tp: OrType =>
          derivedOrType(tp, this(tp.tp1), this(tp.tp2))

        case tp: MatchType =>
          val bound1 = this(tp.bound)
          val scrut1 = atVariance(0)(this(tp.scrutinee))
          derivedMatchType(tp, bound1, scrut1, tp.cases.mapConserve(this))

        case tp: SkolemType =>
          derivedSkolemType(tp, this(tp.info))

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

    /** A version of mapClassInfo which also maps parents and self type */
    protected def mapFullClassInfo(tp: ClassInfo): ClassInfo =
      tp.derivedClassInfo(
        prefix = this(tp.prefix),
        classParents = tp.classParents.mapConserve(this),
        selfInfo = tp.selfInfo match {
          case tp: Type => this(tp)
          case sym => sym
        }
      )

    def andThen(f: Type => Type): TypeMap = new TypeMap {
      override def stopAtStatic = thisMap.stopAtStatic
      def apply(tp: Type) = f(thisMap(tp))
    }
  }

  /** A type map that maps also parents and self type of a ClassInfo */
  abstract class DeepTypeMap(implicit ctx: Context) extends TypeMap {
    override def mapClassInfo(tp: ClassInfo): ClassInfo = {
      val prefix1 = this(tp.prefix)
      val parents1 = tp.parents mapConserve this
      val selfInfo1 = tp.selfInfo match {
        case selfInfo: Type => this(selfInfo)
        case selfInfo => selfInfo
      }
      tp.derivedClassInfo(prefix1, parents1, tp.decls, selfInfo1)
    }
  }

  @sharable object IdentityTypeMap extends TypeMap()(NoContext) {
    override def stopAtStatic: Boolean = true
    def apply(tp: Type): Type = tp
  }

  /** A type map that approximates TypeBounds types depending on
   *  variance.
   *
   *  if variance > 0 : approximate by upper bound
   *     variance < 0 : approximate by lower bound
   *     variance = 0 : propagate bounds to next outer level
   */
  abstract class ApproximatingTypeMap(implicit ctx: Context) extends TypeMap { thisMap =>

    protected def range(lo: Type, hi: Type): Type =
      if (variance > 0) hi
      else if (variance < 0) lo
      else if (lo `eq` hi) lo
      else Range(lower(lo), upper(hi))

    protected def emptyRange = range(defn.NothingType, defn.AnyType)

    protected def isRange(tp: Type): Boolean = tp.isInstanceOf[Range]

    protected def lower(tp: Type): Type = tp match {
      case tp: Range => tp.lo
      case _ => tp
    }

    protected def upper(tp: Type): Type = tp match {
      case tp: Range => tp.hi
      case _ => tp
    }

    protected def rangeToBounds(tp: Type): Type = tp match {
      case Range(lo, hi) => TypeBounds(lo, hi)
      case _ => tp
    }

    protected def expandBounds(tp: TypeBounds): Type =
      range(atVariance(-variance)(reapply(tp.lo)), reapply(tp.hi))

    /** Try to widen a named type to its info relative to given prefix `pre`, where possible.
     *  The possible cases are listed inline in the code.
     */
    def tryWiden(tp: NamedType, pre: Type): Type = pre.member(tp.name) match {
      case d: SingleDenotation =>
        val tp1 = d.info.dealiasKeepAnnots
        tp1.stripAnnots match {
          case TypeAlias(alias) =>
            // if H#T = U, then for any x in L..H, x.T =:= U,
            // hence we can replace with U under all variances
            reapply(alias.rewrapAnnots(tp1))
          case tp: TypeBounds =>
            // If H#T = _ >: S <: U, then for any x in L..H, S <: x.T <: U,
            // hence we can replace with S..U under all variances
            expandBounds(tp)
          case info: SingletonType =>
            // if H#x: y.type, then for any x in L..H, x.type =:= y.type,
            // hence we can replace with y.type under all variances
            reapply(info)
          case _ =>
            NoType
        }
      case _ => NoType
    }

    /** Expand parameter reference corresponding to prefix `pre`;
     *  If the expansion is a wildcard parameter reference, convert its
     *  underlying bounds to a range, otherwise return the expansion.
     */
    def expandParam(tp: NamedType, pre: Type): Type = {
      tp.argForParam(pre) match {
        case arg @ TypeRef(pre, _) if pre.isArgPrefixOf(arg.symbol) =>
          arg.info match {
            case argInfo: TypeBounds => expandBounds(argInfo)
            case argInfo => reapply(arg)
          }
        case arg: TypeBounds => expandBounds(arg)
        case arg => reapply(arg)
      }
    }

    /** Derived selection.
     *  @pre   the (upper bound of) prefix `pre` has a member named `tp.name`.
     */
    override protected def derivedSelect(tp: NamedType, pre: Type): Type =
      if (pre eq tp.prefix) tp
      else pre match {
        case Range(preLo, preHi) =>
          val forwarded =
            if (tp.symbol.is(ClassTypeParam)) expandParam(tp, preHi)
            else tryWiden(tp, preHi)
          forwarded.orElse(
            range(super.derivedSelect(tp, preLo).loBound, super.derivedSelect(tp, preHi).hiBound))
        case _ =>
          super.derivedSelect(tp, pre) match {
            case TypeBounds(lo, hi) => range(lo, hi)
            case tp => tp
          }
      }

    override protected def derivedRefinedType(tp: RefinedType, parent: Type, info: Type): Type =
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
              if (!infoLo.isTypeAlias && !infoHi.isTypeAlias) propagate(infoLo, infoHi)
              else range(defn.NothingType, tp.parent)
            case Range(infoLo, infoHi) =>
              propagate(infoLo, infoHi)
            case _ =>
              tp.derivedRefinedType(parent, tp.refinedName, info)
          }
        }

    override protected def derivedRecType(tp: RecType, parent: Type): Type =
      if (parent eq tp.parent) tp
      else parent match {
        case Range(lo, hi) => range(tp.rebind(lo), tp.rebind(hi))
        case _ => tp.rebind(parent)
      }

    override protected def derivedAlias(tp: AliasingBounds, alias: Type): Type =
      if (alias eq tp.alias) tp
      else alias match {
        case Range(lo, hi) =>
          if (variance > 0) TypeBounds(lo, hi)
          else range(tp.derivedAlias(lo), tp.derivedAlias(hi))
        case _ => tp.derivedAlias(alias)
      }

    override protected def derivedTypeBounds(tp: TypeBounds, lo: Type, hi: Type): Type =
      if ((lo eq tp.lo) && (hi eq tp.hi)) tp
      else if (isRange(lo) || isRange(hi))
        if (variance > 0) TypeBounds(lower(lo), upper(hi))
        else range(TypeBounds(upper(lo), lower(hi)), TypeBounds(lower(lo), upper(hi)))
      else tp.derivedTypeBounds(lo, hi)

    override protected def derivedSuperType(tp: SuperType, thistp: Type, supertp: Type): Type =
      if (isRange(thistp) || isRange(supertp)) emptyRange
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
              else range(defn.NothingType, defn.AnyType)
                // TODO: can we give a better bound than `topType`?
            }
          }
          else tp.derivedAppliedType(tycon, args)
      }

    override protected def derivedAndType(tp: AndType, tp1: Type, tp2: Type): Type =
      if (isRange(tp1) || isRange(tp2)) range(lower(tp1) & lower(tp2), upper(tp1) & upper(tp2))
      else tp.derivedAndType(tp1, tp2)

    override protected def derivedOrType(tp: OrType, tp1: Type, tp2: Type): Type =
      if (isRange(tp1) || isRange(tp2)) range(lower(tp1) | lower(tp2), upper(tp1) | upper(tp2))
      else tp.derivedOrType(tp1, tp2)

    override protected def derivedAnnotatedType(tp: AnnotatedType, underlying: Type, annot: Annotation): Type =
      underlying match {
        case Range(lo, hi) =>
          range(tp.derivedAnnotatedType(lo, annot), tp.derivedAnnotatedType(hi, annot))
        case _ =>
          if (underlying.isBottomType) underlying
          else tp.derivedAnnotatedType(underlying, annot)
      }
    override protected def derivedWildcardType(tp: WildcardType, bounds: Type): WildcardType = {
      tp.derivedWildcardType(rangeToBounds(bounds))
    }

    override protected def derivedSkolemType(tp: SkolemType, info: Type): Type = info match {
      case Range(lo, hi) =>
        range(tp.derivedSkolemType(lo), tp.derivedSkolemType(hi))
      case _ =>
        tp.derivedSkolemType(info)
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

    protected def stopAtStatic: Boolean = true

    def apply(x: T, tp: Type): T

    protected def applyToAnnot(x: T, annot: Annotation): T = x // don't go into annotations

    protected final def applyToPrefix(x: T, tp: NamedType): T =
      atVariance(variance max 0)(this(x, tp.prefix)) // see remark on NamedType case in TypeMap

    def foldOver(x: T, tp: Type): T = {
      record(s"foldOver $getClass")
      record(s"foldOver total")
      tp match {
      case tp: TypeRef =>
        if (stopAtStatic && tp.symbol.isStatic || (tp.prefix `eq` NoPrefix)) x
        else {
          val tp1 = tp.prefix.lookupRefined(tp.name)
          if (tp1.exists) this(x, tp1) else applyToPrefix(x, tp)
        }

      case tp @ AppliedType(tycon, args) =>
        @tailrec def foldArgs(x: T, tparams: List[ParamInfo], args: List[Type]): T =
          if (args.isEmpty || tparams.isEmpty) x
          else {
            val tparam = tparams.head
            val acc = args.head match {
              case arg: TypeBounds => this(x, arg)
              case arg => atVariance(variance * tparam.paramVariance)(this(x, arg))
            }
            foldArgs(acc, tparams.tail, args.tail)
          }
        foldArgs(this(x, tycon), tp.typeParams, args)

      case _: BoundType | _: ThisType => x

      case tp: LambdaType =>
        val restpe = tp.resultType
        val saved = variance
        variance = if (defn.MatchCase.isInstance(restpe)) 0 else -variance
        val y = foldOver(x, tp.paramInfos)
        variance = saved
        this(y, restpe)

      case tp: TermRef =>
        if (stopAtStatic && tp.currentSymbol.isStatic || (tp.prefix `eq` NoPrefix)) x
        else applyToPrefix(x, tp)

      case tp: TypeVar =>
        this(x, tp.underlying)

      case ExprType(restpe) =>
        this(x, restpe)

      case bounds @ TypeBounds(lo, hi) =>
        if (lo eq hi) atVariance(0)(this(x, lo))
        else {
          variance = -variance
          val y = this(x, lo)
          variance = -variance
          this(y, hi)
        }

      case tp: AndType =>
        this(this(x, tp.tp1), tp.tp2)

      case tp: OrType =>
        this(this(x, tp.tp1), tp.tp2)

      case tp: MatchType =>
        val x1 = this(x, tp.bound)
        val x2 = atVariance(0)(this(x1, tp.scrutinee))
        foldOver(x2, tp.cases)

      case AnnotatedType(underlying, annot) =>
        this(applyToAnnot(x, annot), underlying)

      case tp: ProtoType =>
        tp.fold(x, this)

      case tp: RefinedType =>
        this(this(x, tp.parent), tp.refinedInfo)

      case tp: WildcardType =>
        this(x, tp.optBounds)

      case tp @ ClassInfo(prefix, _, _, _, _) =>
        this(x, prefix)

      case tp: JavaArrayType =>
        this(x, tp.elemType)

      case tp: SkolemType =>
        this(x, tp.info)

      case SuperType(thistp, supertp) =>
        this(this(x, thistp), supertp)

      case tp: LazyRef =>
        this(x, tp.ref)

      case tp: RecType =>
        this(x, tp.parent)

      case _ => x
    }}

    @tailrec final def foldOver(x: T, ts: List[Type]): T = ts match {
      case t :: ts1 => foldOver(apply(x, t), ts1)
      case nil => x
    }
  }

  abstract class TypeTraverser(implicit ctx: Context) extends TypeAccumulator[Unit] {
    def traverse(tp: Type): Unit
    def apply(x: Unit, tp: Type): Unit = traverse(tp)
    protected def traverseChildren(tp: Type): Unit = foldOver((), tp)
  }

  class ExistsAccumulator(p: Type => Boolean, forceLazy: Boolean = true)(implicit ctx: Context) extends TypeAccumulator[Boolean] {
    override def stopAtStatic: Boolean = false
    def apply(x: Boolean, tp: Type): Boolean =
      x || p(tp) || (forceLazy || !tp.isInstanceOf[LazyRef]) && foldOver(x, tp)
  }

  class ForeachAccumulator(p: Type => Unit, override val stopAtStatic: Boolean)(implicit ctx: Context) extends TypeAccumulator[Unit] {
    def apply(x: Unit, tp: Type): Unit = foldOver(p(tp), tp)
  }

  class NamedPartsAccumulator(p: NamedType => Boolean, excludeLowerBounds: Boolean = false)
    (implicit ctx: Context) extends TypeAccumulator[mutable.Set[NamedType]] {
    override def stopAtStatic: Boolean = false
    def maybeAdd(x: mutable.Set[NamedType], tp: NamedType): mutable.Set[NamedType] = if (p(tp)) x += tp else x
    val seen: util.HashSet[Type] = new util.HashSet[Type](64) {
      override def hash(x: Type): Int = System.identityHashCode(x)
      override def isEqual(x: Type, y: Type) = x.eq(y)
    }
    def apply(x: mutable.Set[NamedType], tp: Type): mutable.Set[NamedType] =
      if (seen contains tp) x
      else {
        seen.addEntry(tp)
        tp match {
          case tp: TypeRef =>
            foldOver(maybeAdd(x, tp), tp)
          case tp: ThisType =>
            apply(x, tp.tref)
          case NoPrefix =>
            foldOver(x, tp)
          case tp: TermRef =>
            apply(foldOver(maybeAdd(x, tp), tp), tp.underlying)
          case tp: AppliedType =>
            foldOver(x, tp)
          case TypeBounds(lo, hi) =>
            if (!excludeLowerBounds) apply(x, lo)
            apply(x, hi)
          case tp: ParamRef =>
            apply(x, tp.underlying)
          case tp: ConstantType =>
            apply(x, tp.underlying)
          case _ =>
            foldOver(x, tp)
        }
      }
  }

  class isGroundAccumulator(implicit ctx: Context) extends TypeAccumulator[Boolean] {
    def apply(x: Boolean, tp: Type): Boolean = x && {
      tp match {
        case _: TypeParamRef => false
        case tp: TypeVar => apply(x, tp.underlying)
        case tp: AppliedType => tp.isGround(this)
        case _ => foldOver(x, tp)
      }
    }
  }

  class TypeSizeAccumulator(implicit ctx: Context) extends TypeAccumulator[Int] {
    val seen = new java.util.IdentityHashMap[Type, Type]
    def apply(n: Int, tp: Type): Int =
      if (seen.get(tp) != null) n
      else {
        seen.put(tp, tp)
        tp match {
        case tp: AppliedType =>
          foldOver(n + 1, tp)
        case tp: RefinedType =>
          foldOver(n + 1, tp)
        case tp: TypeRef if tp.info.isTypeAlias =>
          apply(n, tp.superType)
        case tp: TypeParamRef =>
          apply(n, ctx.typeComparer.bounds(tp))
        case _ =>
          foldOver(n, tp)
      }
    }
  }

  class CoveringSetAccumulator(implicit ctx: Context) extends TypeAccumulator[Set[Symbol]] {
    val seen = new java.util.IdentityHashMap[Type, Type]
    def apply(cs: Set[Symbol], tp: Type): Set[Symbol] = {
      if (seen.get(tp) != null) cs
      else {
        seen.put(tp, tp)
        tp match {
          case tp if tp.isTopType || tp.isBottomType =>
            cs
          case tp: AppliedType =>
            foldOver(cs + tp.typeSymbol, tp)
          case tp: RefinedType =>
            foldOver(cs + tp.typeSymbol, tp)
          case tp: TypeRef if tp.info.isTypeAlias =>
            apply(cs, tp.superType)
          case tp: TypeRef if tp.typeSymbol.isClass =>
            foldOver(cs + tp.typeSymbol, tp)
          case tp: TermRef =>
            val tsym = if (tp.termSymbol.is(Param)) tp.underlying.typeSymbol else tp.termSymbol
            foldOver(cs + tsym, tp)
          case tp: TypeParamRef =>
            apply(cs, ctx.typeComparer.bounds(tp))
          case other =>
            foldOver(cs, tp)
        }
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

  /** A filter for names of type aliases of a given type */
  object typeAliasNameFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean =
      name.isTypeName && {
        val mbr = pre.nonPrivateMember(name)
        mbr.symbol.isAliasType
      }
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

  // ----- Debug ---------------------------------------------------------

  @sharable var debugTrace: Boolean = false

  val watchList: List[TypeName] = List[String](
  ) map (_.toTypeName)

  def isWatched(tp: Type)(implicit ctx: Context): Boolean = tp match {
    case ref: TypeRef => watchList contains ref.name
    case _ => false
  }

  // ----- Helpers and Decorator implicits --------------------------------------

  implicit def decorateTypeApplications(tpe: Type): TypeApplications = new TypeApplications(tpe)

  implicit class typeListDeco(val tps1: List[Type]) extends AnyVal {
    @tailrec def stableHash: Boolean =
      tps1.isEmpty || tps1.head.stableHash && tps1.tail.stableHash
    @tailrec def equalElements(tps2: List[Type], bs: BinderPairs): Boolean =
      (tps1 `eq` tps2) || {
        if (tps1.isEmpty) tps2.isEmpty
        else tps2.nonEmpty && tps1.head.equals(tps2.head, bs) && tps1.tail.equalElements(tps2.tail, bs)
      }
  }

  private val keepAlways: AnnotatedType => Context => Boolean = _ => _ => true
  private val keepNever: AnnotatedType => Context => Boolean = _ => _ => false
  private val keepIfRefining: AnnotatedType => Context => Boolean = tp => ctx => tp.isRefining(ctx)

  val isBounds: Type => Boolean = _.isInstanceOf[TypeBounds]
}
