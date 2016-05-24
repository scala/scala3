package dotty.tools
package dotc
package core

import util.common._
import Symbols._
import Flags._
import Names._
import StdNames._, NameOps._
import Scopes._
import Constants._
import Contexts._
import Annotations._
import SymDenotations._
import Decorators._
import Denotations._
import Periods._
import util.Positions.Position
import util.Stats._
import util.{DotClass, SimpleMap}
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
import config.Printers._
import annotation.tailrec
import Flags.FlagSet
import language.implicitConversions
import scala.util.hashing.{ MurmurHash3 => hashing }

object Types {

  @sharable private var nextId = 0

  implicit def eqType: Eq[Type, Type] = Eq

  /** The class of types.
   *  The principal subclasses and sub-objects are as follows:
   *
   *  Type -+- ProxyType --+- NamedType ----+--- TypeRef
   *        |              |                 \
   *        |              +- SingletonType-+-+- TermRef
   *        |              |                |
   *        |              |                +--- ThisType
   *        |              |                +--- SuperType
   *        |              |                +--- ConstantType
   *        |              |                +--- MethodParam
   *        |              |                +----RefinedThis
   *        |              |                +--- SkolemType
   *        |              +- PolyParam
   *        |              +- RefinedType
   *        |              +- TypeBounds
   *        |              +- ExprType
   *        |              +- AnnotatedType
   *        |              +- TypeVar
   *        |
   *        +- GroundType -+- AndType
   *                       +- OrType
   *                       +- MethodType -----+- ImplicitMethodType
   *                       |                  +- JavaMethodType
   *                       +- PolyType
   *                       +- ClassInfo
   *                       |
   *                       +- NoType
   *                       +- NoPrefix
   *                       +- ErrorType
   *                       +- WildcardType
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

    /** Does this type denote a stable reference (i.e. singleton type)? */
    final def isStable(implicit ctx: Context): Boolean = stripTypeVar match {
      case tp: TermRef => tp.termSymbol.isStable && tp.prefix.isStable
      case _: SingletonType | NoPrefix => true
      case tp: RefinedType => tp.parent.isStable
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
          case _ =>  this1.symbol eq sym
        }
      case this1: RefinedType =>
        this1.parent.isRef(sym)
      case _ =>
        false
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
      def loop(tp: Type) = tp match {
        case tp: TypeRef =>
          val sym = tp.symbol
          if (sym.isClass) sym.derivesFrom(cls) else tp.underlying.derivesFrom(cls)
        case tp: TypeProxy =>
          tp.underlying.derivesFrom(cls)
        case tp: AndType =>
          tp.tp1.derivesFrom(cls) || tp.tp2.derivesFrom(cls)
        case tp: OrType =>
          tp.tp1.derivesFrom(cls) && tp.tp2.derivesFrom(cls)
        case tp: JavaArrayType =>
          cls == defn.ObjectClass
        case _ =>
          false
      }
      cls == defn.AnyClass || loop(this)
    }

    /** Is this type guaranteed not to have `null` as a value?
     *  For the moment this is only true for modules, but it could
     *  be refined later.
     */
    final def isNotNull(implicit ctx: Context): Boolean =
      classSymbol is ModuleClass

    /** Is this type produced as a repair for an error? */
    final def isError(implicit ctx: Context): Boolean = stripTypeVar match {
      case ErrorType => true
      case tp => (tp.typeSymbol is Erroneous) || (tp.termSymbol is Erroneous)
    }

    /** Is some part of this type produced as a repair for an error? */
    final def isErroneous(implicit ctx: Context): Boolean = existsPart(_.isError)

    /** Does the type carry an annotation that is an instance of `cls`? */
    final def hasAnnotation(cls: ClassSymbol)(implicit ctx: Context): Boolean = stripTypeVar match {
      case AnnotatedType(tp, annot) => (annot matches cls) || (tp hasAnnotation cls)
      case _ => false
    }

    /** Does this type occur as a part of type `that`? */
    final def occursIn(that: Type)(implicit ctx: Context): Boolean =
      that existsPart (this == _)

    /** Is this a type of a repeated parameter? */
    def isRepeatedParam(implicit ctx: Context): Boolean =
      typeSymbol eq defn.RepeatedParamClass

    /** Does this type carry an UnsafeNonvariant annotation? */
    final def isUnsafeNonvariant(implicit ctx: Context): Boolean = this match {
      case AnnotatedType(_, annot) => annot.symbol == defn.UnsafeNonvariantAnnot
      case _ => false
    }

    /** Does this type have an UnsafeNonvariant annotation on one of its parts? */
    final def hasUnsafeNonvariant(implicit ctx: Context): Boolean =
      new HasUnsafeNonAccumulator().apply(false, this)

    /** Is this the type of a method that has a repeated parameter type as
     *  last parameter type?
     */
    def isVarArgsMethod(implicit ctx: Context): Boolean = this match {
      case tp: PolyType => tp.resultType.isVarArgsMethod
      case MethodType(_, paramTypes) => paramTypes.nonEmpty && paramTypes.last.isRepeatedParam
      case _ => false
    }

    /** Is this an alias TypeBounds? */
    def isAlias: Boolean = this.isInstanceOf[TypeAlias]

    /** Is this type a transitive refinement of the given type?
     *  This is true if the type consists of 0 or more refinements or other
     *  non-singleton proxies that lead to the `prefix` type. ClassInfos with
     *  the same class are counted as equal for this purpose.
     */
    def refines(prefix: Type)(implicit ctx: Context): Boolean = {
      val prefix1 = prefix.dealias
      def loop(tp: Type): Boolean =
        (tp eq prefix1) || {
          tp match {
            case base: ClassInfo =>
              prefix1 match {
                case prefix1: ClassInfo => base.cls eq prefix1.cls
                case _ => false
              }
            case base: SingletonType => false
            case base: TypeProxy => loop(base.underlying)
            case _ => false
          }
        }
      loop(this)
    }

// ----- Higher-order combinators -----------------------------------

    /** Returns true if there is a part of this type that satisfies predicate `p`.
     */
    final def existsPart(p: Type => Boolean)(implicit ctx: Context): Boolean =
      new ExistsAccumulator(p).apply(false, this)

    /** Returns true if all parts of this type satisfy predicate `p`.
     */
    final def forallParts(p: Type => Boolean)(implicit ctx: Context): Boolean =
      !existsPart(!p(_))

    /** Performs operation on all parts of this type */
    final def foreachPart(p: Type => Unit)(implicit ctx: Context): Unit =
      new ForeachAccumulator(p).apply((), this)

    /** The parts of this type which are type or term refs */
    final def namedParts(implicit ctx: Context): collection.Set[NamedType] =
      namedPartsWith(alwaysTrue)

    /** The parts of this type which are type or term refs and which
     *  satisfy predicate `p`.
     */
    def namedPartsWith(p: NamedType => Boolean)(implicit ctx: Context): collection.Set[NamedType] =
      new NamedPartsAccumulator(p).apply(mutable.LinkedHashSet(), this)

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
    final def typeSymbol(implicit ctx: Context): Symbol = this match {
      case tp: TypeRef => tp.symbol
      case tp: ClassInfo => tp.cls
//    case ThisType(cls) => cls // needed?
      case tp: SingletonType => NoSymbol
      case tp: TypeProxy => tp.underlying.typeSymbol
      case _ => NoSymbol
    }

    /** The least class or trait of which this type is a subtype, or
     *  NoSymbol if none exists (either because this type is not a
     *  value type, or because superclasses are ambiguous).
     */
    final def classSymbol(implicit ctx: Context): Symbol = this match {
      case ConstantType(constant) =>
        constant.tpe.classSymbol
      case tp: TypeRef =>
        val sym = tp.symbol
        if (sym.isClass) sym else tp.underlying.classSymbol
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
        if (sym.isClass) sym.asClass :: Nil else tp.underlying.classSymbols
      case tp: TypeProxy =>
        tp.underlying.classSymbols
      case AndType(l, r) =>
        l.classSymbols union r.classSymbols
      case OrType(l, r) =>
        l.classSymbols intersect r.classSymbols // TODO does not conform to spec
      case _ =>
        Nil
    }

    /** The term symbol associated with the type */
    final def termSymbol(implicit ctx: Context): Symbol = this match {
      case tp: TermRef => tp.symbol
      case tp: TypeProxy => tp.underlying.termSymbol
      case _ => NoSymbol
    }

    /** The base classes of this type as determined by ClassDenotation
     *  in linearization order, with the class itself as first element.
     *  For AndTypes/OrTypes, the union/intersection of the operands' baseclasses.
     *  Inherited by all type proxies. `Nil` for all other types.
     */
    final def baseClasses(implicit ctx: Context): List[ClassSymbol] = track("baseClasses") {
      this match {
        case tp: TypeProxy =>
          tp.underlying.baseClasses
        case tp: ClassInfo =>
          tp.cls.baseClasses
        case AndType(tp1, tp2) =>
          tp1.baseClasses union tp2.baseClasses
        case OrType(tp1, tp2) =>
          tp1.baseClasses intersect tp2.baseClasses
        case _ => Nil
      }
    }

// ----- Member access -------------------------------------------------

    /** The scope of all declarations of this type.
     *  Defined by ClassInfo, inherited by type proxies.
     *  Empty scope for all other types.
     */
    final def decls(implicit ctx: Context): Scope = this match {
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
    final def findDecl(name: Name, excluded: FlagSet)(implicit ctx: Context): Denotation = this match {
      case tp: ClassInfo =>
        tp.decls.denotsNamed(name).filterExcluded(excluded).toDenot(NoPrefix)
      case tp: TypeProxy =>
        tp.underlying.findDecl(name, excluded)
      case ErrorType =>
        ctx.newErrorSymbol(classSymbol orElse defn.RootClass, name)
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
          tp.typeRef
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
        case tp: RefinedType =>
          if (name eq tp.refinedName) goRefined(tp) else go(tp.parent)
        case tp: ThisType =>
          goThis(tp)
        case tp: TypeRef =>
          tp.denot.findMember(name, pre, excluded)
        case tp: TermRef =>
          go (tp.underlying match {
            case mt: MethodType
            if mt.paramTypes.isEmpty && (tp.symbol is Stable) => mt.resultType
            case tp1 => tp1
          })
        case tp: PolyParam =>
          goParam(tp)
        case tp: TypeProxy =>
          go(tp.underlying)
        case tp: ClassInfo =>
          tp.cls.findMember(name, pre, excluded)
        case AndType(l, r) =>
          goAnd(l, r)
        case OrType(l, r) =>
          goOr(l, r)
        case tp: JavaArrayType =>
          defn.ObjectType.findMember(name, pre, excluded)
        case ErrorType =>
          ctx.newErrorSymbol(pre.classSymbol orElse defn.RootClass, name)
        case _ =>
          NoDenotation
      }
      def goRefined(tp: RefinedType) = {
        val pdenot = go(tp.parent)
        val rinfo =
          if (tp.refinementRefersToThis) tp.refinedInfo.substRefinedThis(tp, pre)
          else tp.refinedInfo
        if (name.isTypeName) { // simplified case that runs more efficiently
          val jointInfo =
            if (rinfo.isAlias) rinfo
            else if (pdenot.info.isAlias) pdenot.info
            else if (ctx.pendingMemberSearches.contains(name)) pdenot.info safe_& rinfo
            else
              try pdenot.info & rinfo
              catch {
                case ex: CyclicReference =>
                  // happens for tests/pos/sets.scala. findMember is called from baseTypeRef.
                  // The & causes a subtype check which calls baseTypeRef again with the same
                  // superclass. In the observed case, the superclass was Any, and
                  // the special shortcut for Any in derivesFrom was as yet absent. To reproduce,
                  // remove the special treatment of Any in derivesFrom and compile
                  // sets.scala.
                  pdenot.info safe_& rinfo
              }
          pdenot.asSingleDenotation.derivedSingleDenotation(pdenot.symbol, jointInfo)
        } else {
          pdenot & (
            new JointRefDenotation(NoSymbol, rinfo, Period.allInRun(ctx.runId)),
            pre,
            safeIntersection = ctx.pendingMemberSearches.contains(name))
        }
      }
      def goThis(tp: ThisType) = {
        val d = go(tp.underlying)
        if (d.exists)
          if ((pre eq tp) && d.symbol.is(NamedTypeParam) && (d.symbol.owner eq tp.cls))
            // If we look for a named type parameter `P` in `C.this.P`, looking up
            // the fully applied self type of `C` will give as an info the alias type
            // `P = this.P`. We need to return a denotation with the underlying bounds instead.
            d.symbol.denot
          else d
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
      def goParam(tp: PolyParam) = {
        val next = tp.underlying
        ctx.typerState.constraint.entry(tp) match {
          case bounds: TypeBounds if bounds ne next =>
            ctx.typerState.ephemeral = true
            go(bounds.hi)
          case _ =>
            go(next)
        }
      }
      def goAnd(l: Type, r: Type) = {
        go(l) & (go(r), pre, safeIntersection = ctx.pendingMemberSearches.contains(name))
      }
      def goOr(l: Type, r: Type) = go(l) | (go(r), pre)

      { val recCount = ctx.findMemberCount + 1
        ctx.findMemberCount = recCount
        if (recCount >= Config.LogPendingFindMemberThreshold)
          ctx.pendingMemberSearches = name :: ctx.pendingMemberSearches
      }

      try go(this)
      catch {
        case ex: Throwable =>
          core.println(i"findMember exception for $this member $name")
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
          (name, buf) => buf ++= member(name).altsWith(_ is Deferred))
    }

    /** The set of abstract type members of this type. */
    final def abstractTypeMembers(implicit ctx: Context): Seq[SingleDenotation] = track("abstractTypeMembers") {
      memberDenots(abstractTypeNameFilter,
          (name, buf) => buf += member(name).asSingleDenotation)
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

    /** Is this type a legal type for a member that overrides another
     *  member of type `that`? This is the same as `<:<`, except that
     *  the types ()T and => T are identified, and T is seen as overriding
     *  either type.
     */
    final def overrides(that: Type)(implicit ctx: Context) = {
      def result(tp: Type): Type = tp match {
        case ExprType(_) | MethodType(Nil, _) => tp.resultType
        case _ => tp
      }
      (this frozen_<:< that) || {
        val rthat = result(that)
        (rthat ne that) && (result(this) frozen_<:< rthat)
      }
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
    final def baseTypeRef(base: Symbol)(implicit ctx: Context): Type = /*ctx.traceIndented(s"$this baseTypeRef $base")*/ /*>|>*/ track("baseTypeRef") /*<|<*/ {
      base.denot match {
        case classd: ClassDenotation => classd.baseTypeRefOf(this)
        case _ => NoType
      }
    }

    def & (that: Type)(implicit ctx: Context): Type = track("&") {
      ctx.typeComparer.glb(this, that)
    }

    /** Safer version of `&`.
     *
     *  This version does not simplify the upper bound of the intersection of
     *  two TypeBounds. The simplification done by `&` requires subtyping checks
     *  which may end up calling `&` again, in most cases this should be safe
     *  but because of F-bounded types, this can result in an infinite loop
     *  (which will be masked unless `-Yno-deep-subtypes` is enabled).
     */
    def safe_& (that: Type)(implicit ctx: Context): Type = (this, that) match {
      case (TypeBounds(lo1, hi1), TypeBounds(lo2, hi2)) => TypeBounds(lo1 | lo2, AndType(hi1, hi2))
      case _ => this & that
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

    /** Follow aliases and dereferences LazyRefs and instantiated TypeVars until type
     *  is no longer alias type, LazyRef, or instantiated type variable.
     */
    final def dealias(implicit ctx: Context): Type = this match {
      case tp: TypeRef =>
        if (tp.symbol.isClass) tp
        else tp.info match {
          case TypeAlias(tp) => tp.dealias
          case _ => tp
        }
      case tp: TypeVar =>
        val tp1 = tp.instanceOpt
        if (tp1.exists) tp1.dealias else tp
      case tp: LazyRef =>
        tp.ref.dealias
      case tp: AnnotatedType =>
        tp.derivedAnnotatedType(tp.tpe.dealias, tp.annot)
      case tp => tp
    }

    /** If this is a TypeAlias type, its alias otherwise this type itself */
    final def followTypeAlias(implicit ctx: Context): Type = this match {
      case TypeAlias(alias) => alias
      case _ => this
    }

    /** Perform successive widenings and dealiasings until none can be applied anymore */
    final def widenDealias(implicit ctx: Context): Type = {
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

    /** If this is a (possibly aliased, annotated, and/or parameterized) reference to
     *  a class, the class type ref, otherwise NoType.
     *  @param  refinementOK   If `true` we also skip non-parameter refinements.
     */
    def underlyingClassRef(refinementOK: Boolean)(implicit ctx: Context): Type = dealias match {
      case tp: TypeRef =>
        if (tp.symbol.isClass) tp
        else if (tp.symbol.isAliasType) tp.underlying.underlyingClassRef(refinementOK)
        else NoType
      case tp: AnnotatedType => tp.underlying.underlyingClassRef(refinementOK)
      case tp: RefinedType =>
        def isParamName = tp.classSymbol.typeParams.exists(_.name == tp.refinedName)
        if (refinementOK || isParamName) tp.underlying.underlyingClassRef(refinementOK)
        else NoType
      case _ => NoType
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
     *  (2) The refinement is a fully instantiated type lambda, and the projected name is "$apply".
     *      In this case the rhs of the apply is returned with all references to lambda argument types
     *      substituted by their definitions.
     *
     *  (*) normalizes means: follow instantiated typevars and aliases.
     */
    def lookupRefined(name: Name)(implicit ctx: Context): Type = {
      def loop(pre: Type): Type = pre.stripTypeVar match {
        case pre: RefinedType =>
          object instantiate extends TypeMap {
            var isSafe = true
            def apply(tp: Type): Type =
              if (!isSafe) tp
              else tp match {
              case TypeRef(RefinedThis(`pre`), name) if name.isHkArgName =>
                member(name).info match {
                  case TypeAlias(alias) => alias
                  case _ => isSafe = false; tp
                }
              case tp: TypeVar if !tp.inst.exists =>
                isSafe = false
                tp
              case _ =>
                mapOver(tp)
            }
          }
          def instArg(tp: Type): Type = tp match {
            case tp @ TypeAlias(TypeRef(RefinedThis(`pre`), name)) if name.isHkArgName =>
              member(name).info match {
                case TypeAlias(alias) => tp.derivedTypeAlias(alias) // needed to keep variance
                case bounds => bounds
              }
            case _ =>
              instantiate(tp)
          }
          def instTop(tp: Type): Type = tp.stripTypeVar match {
            case tp: RefinedType =>
              tp.derivedRefinedType(instTop(tp.parent), tp.refinedName, instArg(tp.refinedInfo))
            case _ =>
              instantiate(tp)
          }
          /** Reduce rhs of $hkApply to make it stand alone */
          def betaReduce(tp: Type) = {
            val reduced = instTop(tp)
            if (instantiate.isSafe) reduced else NoType
          }
          pre.refinedInfo match {
            case TypeAlias(alias) =>
              if (pre.refinedName ne name) loop(pre.parent)
              else if (!pre.refinementRefersToThis) alias
              else alias match {
                case TypeRef(RefinedThis(`pre`), aliasName) => lookupRefined(aliasName) // (1)
                case _ => if (name == tpnme.hkApply) betaReduce(alias) else NoType // (2) // ### use TypeApplication's betaReduce
              }
            case _ => loop(pre.parent)
          }
        case RefinedThis(binder) =>
          binder.lookupRefined(name)
        case SkolemType(tp) =>
          tp.lookupRefined(name)
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
    final def normalizedPrefix(implicit ctx: Context): Type = this match {
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
    def parents(implicit ctx: Context): List[TypeRef] = this match {
      case tp: TypeProxy => tp.underlying.parents
      case _ => List()
    }

    /** The full parent types, including all type arguments */
    def parentsWithArgs(implicit ctx: Context): List[Type] = this match {
      case tp: TypeProxy => tp.underlying.parentsWithArgs
      case _ => List()
    }

    /** The first parent of this type, AnyRef if list of parents is empty */
    def firstParent(implicit ctx: Context): TypeRef = parents match {
      case p :: _ => p
      case _ => defn.AnyType
    }

    /** the self type of the underlying classtype */
    def givenSelfType(implicit ctx: Context): Type = this match {
      case tp @ RefinedType(parent, name) => tp.wrapIfMember(parent.givenSelfType)
      case tp: ThisType => tp.tref.givenSelfType
      case tp: TypeProxy => tp.underlying.givenSelfType
      case _ => NoType
    }

    /** The parameter types of a PolyType or MethodType, Empty list for others */
    final def paramTypess(implicit ctx: Context): List[List[Type]] = this match {
      case mt: MethodType => mt.paramTypes :: mt.resultType.paramTypess
      case pt: PolyType => pt.resultType.paramTypess
      case _ => Nil
    }

    /** The parameter names of a PolyType or MethodType, Empty list for others */
    final def paramNamess(implicit ctx: Context): List[List[TermName]] = this match {
      case mt: MethodType => mt.paramNames :: mt.resultType.paramNamess
      case pt: PolyType => pt.resultType.paramNamess
      case _ => Nil
    }


    /** The parameter types in the first parameter section of a PolyType or MethodType, Empty list for others */
    final def firstParamTypes(implicit ctx: Context): List[Type] = this match {
      case mt: MethodType => mt.paramTypes
      case pt: PolyType => pt.resultType.firstParamTypes
      case _ => Nil
    }

    /** Is this either not a method at all, or a parameterless method? */
    final def isParameterless(implicit ctx: Context): Boolean = this match {
      case mt: MethodType => false
      case pt: PolyType => pt.resultType.isParameterless
      case _ => true
    }

    /** The resultType of a PolyType, MethodType, or ExprType, the type itself for others */
    def resultType(implicit ctx: Context): Type = this

    /** The final result type of a PolyType, MethodType, or ExprType, after skipping
     *  all parameter sections, the type itself for all others.
     */
    def finalResultType(implicit ctx: Context): Type = resultType match {
      case mt: MethodType => mt.resultType.finalResultType
      case pt: PolyType => pt.resultType.finalResultType
      case _ => resultType
    }

    /** This type seen as a TypeBounds */
    final def bounds(implicit ctx: Context): TypeBounds = this match {
      case tp: TypeBounds => tp
      case ci: ClassInfo => TypeAlias(ci.typeRef)
      case wc: WildcardType =>
        wc.optBounds match {
          case bounds: TypeBounds => bounds
          case NoType => TypeBounds.empty
        }
      case _ => TypeAlias(this)
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

    /** Substitute all types of the form `PolyParam(from, N)` by
     *  `PolyParam(to, N)`.
     */
    final def subst(from: BindingType, to: BindingType)(implicit ctx: Context): Type =
      ctx.subst(this, from, to, null)

    /** Substitute all occurrences of `This(cls)` by `tp` */
    final def substThis(cls: ClassSymbol, tp: Type)(implicit ctx: Context): Type =
      ctx.substThis(this, cls, tp, null)

    /** As substThis, but only is class is a static owner (i.e. a globally accessible object) */
    final def substThisUnlessStatic(cls: ClassSymbol, tp: Type)(implicit ctx: Context): Type =
      if (cls.isStaticOwner) this else ctx.substThis(this, cls, tp, null)

    /** Substitute all occurrences of `SkolemType(binder)` by `tp` */
    final def substRefinedThis(binder: Type, tp: Type)(implicit ctx: Context): Type =
      ctx.substRefinedThis(this, binder, tp, null)

    /** Substitute a bound type by some other type */
    final def substParam(from: ParamType, to: Type)(implicit ctx: Context): Type =
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
     *  @param drop  The number of trailing parameters that should be dropped
     *               when forming the function type.
     */
    def toFunctionType(dropLast: Int = 0)(implicit ctx: Context): Type = this match {
      case mt @ MethodType(_, formals) if !mt.isDependent || ctx.mode.is(Mode.AllowDependentFunctions) =>
        val formals1 = if (dropLast == 0) formals else formals dropRight dropLast
        defn.FunctionOf(
            formals1 mapConserve (_.underlyingIfRepeated(mt.isJava)), mt.resultType)
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

    /** Approximations of union types: We replace a union type Tn | ... | Tn
     *  by the smallest intersection type of baseclass instances of T1,...,Tn.
     *  Example: Given
     *
     *      trait C[+T]
     *      trait D
     *      class A extends C[A] with D
     *      class B extends C[B] with D with E
     *
     *  we approximate `A | B` by `C[A | B] with D`
     *
     *  As a second measure we also homogenize refinements containing
     *  type variables. For instance, if `A` is an instantiatable type variable,
     *  then
     *
     *      ArrayBuffer[Int] | ArrayBuffer[A]
     *
     *  is approximated by instantiating `A` to `Int` and returning `ArrayBuffer[Int]`
     *  instead of `ArrayBuffer[_ >: Int | A <: Int & A]`
     */
    def approximateUnion(implicit ctx: Context) = ctx.approximateUnion(this)

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
        if (myHash == HashUnknown) myHash = HashUnknownAlt
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
        if (myHash == HashUnknown) myHash = HashUnknownAlt
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

  /** A marker trait for types that apply only to term symbols */
  trait TermType extends Type

  /** A marker trait for types that can be types of values or prototypes of value types */
  trait ValueTypeOrProto extends TermType

  /** A marker trait for types that can be types of values */
  trait ValueType extends ValueTypeOrProto

  /** A marker trait for types that are guaranteed to contain only a
   *  single non-null value (they might contain null in addition).
   */
  trait SingletonType extends TypeProxy with ValueType {
    def isOverloaded(implicit ctx: Context) = false
  }

  /** A marker trait for types that bind other types that refer to them.
   *  Instances are: PolyType, MethodType, RefinedType.
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
  abstract class NamedType extends CachedProxyType with ValueType {

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
              if (newd.exists) newd else d.staleSymbolError
            }
          case d =>
            if (d.validFor.runId != ctx.period.runId) loadDenot
            else {
              val newd = d.currentIfExists
              if (newd ne NotDefinedHereDenotation) newd else loadDenot
            }
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
        }
        d
      }
      finally ctx.typerState.ephemeral |= savedEphemeral
    }

    /** A member of `prefix` (disambiguated by `d.signature`) or, if none was found, `d.current`. */
    private def recomputeMember(d: SymDenotation)(implicit ctx: Context): Denotation =
      asMemberOf(prefix) match {
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
      def selfTypeOf(sym: Symbol) = sym.owner.info match {
        case info: ClassInfo => info.givenSelfType
        case _ => NoType
      }
      assert(
        (lastSymbol eq sym) ||
        (lastSymbol eq null) || {
          val lastDefRunId = lastDenotation match {
            case d: SymDenotation => d.validFor.runId
            case _ => lastSymbol.defRunId
          }
          (lastDefRunId != sym.defRunId) ||
          (lastDefRunId == NoRunId)
        } ||
        (lastSymbol.infoOrCompleter == ErrorType ||
        sym.owner != lastSymbol.owner &&
        (sym.owner.derivesFrom(lastSymbol.owner) ||
         selfTypeOf(sym).derivesFrom(lastSymbol.owner) ||
         selfTypeOf(lastSymbol).derivesFrom(sym.owner))),
        s"""data race? overwriting symbol of type ${this.show},
           |long form = $this of class ${this.getClass},
           |last sym id = ${lastSymbol.id}, new sym id = ${sym.id},
           |last owner = ${lastSymbol.owner}, new owner = ${sym.owner},
           |period = ${ctx.phase} at run ${ctx.runId}""".stripMargin)
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
      val d = asMemberOf(prefix)
      if (d.exists || ctx.phaseId == FirstPhaseId || !lastDenotation.isInstanceOf[SymDenotation])
        d
      else { // name has changed; try load in earlier phase and make current
        val d = loadDenot(ctx.withPhase(ctx.phaseId - 1)).current
        if (d.exists) d
        else throw new Error(s"failure to reload $this of class $getClass")
      }
    }

    protected def asMemberOf(prefix: Type)(implicit ctx: Context) =
      if (name.isShadowedName) prefix.nonPrivateMember(name.revertShadowed)
      else prefix.member(name)

    /** (1) Reduce a type-ref `W # X` or `W { ... } # U`, where `W` is a wildcard type
     *  to an (unbounded) wildcard type.
     *
     *  (2) Reduce a type-ref `T { X = U; ... } # X`  to   `U`
     *  provided `U` does not refer with a RefinedThis to the
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

    /** A selection of the same kind, but with potentially a differet prefix.
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
      else if (isType) {
        val res = prefix.lookupRefined(name)
        if (res.exists) res
        else if (name == tpnme.hkApply && prefix.classNotLambda) {
          // After substitution we might end up with a type like
          // `C { type hk$0 = T0; ...; type hk$n = Tn } # $Apply`
          // where C is a class. In that case we eta expand `C`.
          if (defn.isBottomType(prefix)) prefix.classSymbol.typeRef
          else derivedSelect(prefix.EtaExpandCore)
        }
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
      else newLikeThis(prefix)

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
    final def shadowed(implicit ctx: Context): NamedType =
      NamedType(prefix, name.shadowedName)

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

    override def newLikeThis(prefix: Type)(implicit ctx: Context): TermRef = {
      val candidate = TermRef.withSig(prefix, name, sig)
      if (symbol.exists && !candidate.symbol.exists) { // recompute from previous symbol
        val ownSym = symbol
        val newd = asMemberOf(prefix)
        candidate.withDenot(newd.suchThat(_.signature == ownSym.signature))
      }
      else candidate
    }

    override def equals(that: Any) = that match {
      case that: TermRefWithSignature =>
        this.prefix == that.prefix &&
        this.name == that.name &&
        this.sig == that.sig
      case _ =>
        false
    }
    override def computeHash = doHash((name, sig), prefix)
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
    def checkProjection(prefix: Type, name: TypeName)(implicit ctx: Context) =
      if (name == tpnme.hkApply && prefix.classNotLambda)
        assert(false, s"bad type : $prefix.$name does not allow $$Apply projection")

    /** Create type ref with given prefix and name */
    def apply(prefix: Type, name: TypeName)(implicit ctx: Context): TypeRef = {
      if (Config.checkProjections) checkProjection(prefix, name)
      ctx.uniqueNamedTypes.enterIfNew(prefix, name).asInstanceOf[TypeRef]
    }

    /** Create type ref to given symbol */
    def apply(prefix: Type, sym: TypeSymbol)(implicit ctx: Context): TypeRef =
      withSymAndName(prefix, sym, sym.name)

    /** Create a non-member type ref  (which cannot be reloaded using `member`),
     *  with given prefix, name, and symbol.
     */
    def withFixedSym(prefix: Type, name: TypeName, sym: TypeSymbol)(implicit ctx: Context): TypeRef = {
      if (Config.checkProjections) checkProjection(prefix, name)
      unique(new TypeRefWithFixedSym(prefix, name, sym))
    }

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

  case class LazyRef(refFn: () => Type) extends UncachedProxyType with ValueType {
    private var myRef: Type = null
    private var computed = false
    def ref = {
      if (computed) assert(myRef != null)
      else {
        computed = true
        myRef = refFn()
      }
      myRef
    }
    def evaluating = computed && myRef == null
    override def underlying(implicit ctx: Context) = ref
    override def toString = s"LazyRef($ref)"
    override def equals(other: Any) = other match {
      case other: LazyRef => this.ref.equals(other.ref)
      case _ => false
    }
    override def hashCode = ref.hashCode + 37
  }

  // --- Refined Type ---------------------------------------------------------

  /** A refined type parent { refinement }
   *  @param refinedName  The name of the refinement declaration
   *  @param infoFn: A function that produces the info of the refinement declaration,
   *                 given the refined type itself.
   */
  abstract case class RefinedType(parent: Type, refinedName: Name)
    extends CachedProxyType with BindingType with ValueType {

    val refinedInfo: Type

    private var refinementRefersToThisCache: Boolean = _
    private var refinementRefersToThisKnown: Boolean = false

    def refinementRefersToThis(implicit ctx: Context): Boolean = {
      if (!refinementRefersToThisKnown) {
        refinementRefersToThisCache = refinedInfo.containsRefinedThis(this)
        refinementRefersToThisKnown = true
      }
      refinementRefersToThisCache
    }

    override def underlying(implicit ctx: Context) = parent

    private def badInst =
      throw new AssertionError(s"bad instantiation: $this")

    def checkInst(implicit ctx: Context): this.type = {
      if (refinedName == tpnme.hkApply)
        parent.stripTypeVar match {
          case RefinedType(_, name) if name.isHkArgName => // ok
          case _ => badInst
        }
      this
    }

    def derivedRefinedType(parent: Type, refinedName: Name, refinedInfo: Type)(implicit ctx: Context): RefinedType =
      if ((parent eq this.parent) && (refinedName eq this.refinedName) && (refinedInfo eq this.refinedInfo)) this
      else RefinedType(parent, refinedName, rt => refinedInfo.substRefinedThis(this, RefinedThis(rt)))

    /** Add this refinement to `parent`, provided If `refinedName` is a member of `parent`. */
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
    override def toString = s"RefinedType($parent, $refinedName, $refinedInfo | $hashCode)"
  }

  class CachedRefinedType(parent: Type, refinedName: Name, infoFn: RefinedType => Type) extends RefinedType(parent, refinedName) {
    val refinedInfo = infoFn(this)
  }

  class PreHashedRefinedType(parent: Type, refinedName: Name, override val refinedInfo: Type, hc: Int)
  extends RefinedType(parent, refinedName) {
    myHash = hc
    override def computeHash = unsupported("computeHash")
  }

  object RefinedType {
    def make(parent: Type, names: List[Name], infoFns: List[RefinedType => Type])(implicit ctx: Context): Type =
      if (names.isEmpty) parent
      else make(RefinedType(parent, names.head, infoFns.head), names.tail, infoFns.tail)

    def apply(parent: Type, name: Name, infoFn: RefinedType => Type)(implicit ctx: Context): RefinedType = {
      assert(!ctx.erasedTypes || ctx.mode.is(Mode.Printing))
      ctx.base.uniqueRefinedTypes.enterIfNew(new CachedRefinedType(parent, name, infoFn)).checkInst
    }

    def apply(parent: Type, name: Name, info: Type)(implicit ctx: Context): RefinedType = {
      assert(!ctx.erasedTypes)
      ctx.base.uniqueRefinedTypes.enterIfNew(parent, name, info).checkInst
    }
  }

  // --- AndType/OrType ---------------------------------------------------------------

  trait AndOrType extends ValueType { // todo: check where we can simplify using AndOrType
    def tp1: Type
    def tp2: Type
    def isAnd: Boolean
    def derivedAndOrType(tp1: Type, tp2: Type)(implicit ctx: Context): Type  // needed?
  }

  abstract case class AndType(tp1: Type, tp2: Type) extends CachedGroundType with AndOrType {

    def isAnd = true

    def derivedAndType(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else AndType.make(tp1, tp2)

    def derived_& (tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else tp1 & tp2

    def derivedAndOrType(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      derivedAndType(tp1, tp2)

    override def computeHash = doHash(tp1, tp2)
  }

  final class CachedAndType(tp1: Type, tp2: Type) extends AndType(tp1, tp2)

  object AndType {
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context) = {
      assert(tp1.isInstanceOf[ValueType] && tp2.isInstanceOf[ValueType])
      unchecked(tp1, tp2)
    }
    def unchecked(tp1: Type, tp2: Type)(implicit ctx: Context) = {
      assertUnerased()
      unique(new CachedAndType(tp1, tp2))
    }
    def make(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if ((tp1 eq tp2) || (tp2 eq defn.AnyType))
        tp1
      else if (tp1 eq defn.AnyType)
        tp2
      else
        apply(tp1, tp2)
  }

  abstract case class OrType(tp1: Type, tp2: Type) extends CachedGroundType with AndOrType {
    assert(tp1.isInstanceOf[ValueType] && tp2.isInstanceOf[ValueType])
    def isAnd = false

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
      if (tp1 eq tp2) tp1 else apply(tp1, tp2)
  }

  // ----- Method types: MethodType/ExprType/PolyType -------------------------------

  // Note: method types are cached whereas poly types are not. The reason
  // is that most poly types are cyclic via poly params,
  // and therefore two different poly types would never be equal.

  /** A trait that mixes in functionality for signature caching */
  trait MethodicType extends Type {

    private[this] var mySignature: Signature = _
    private[this] var mySignatureRunId: Int = NoRunId

    protected def computeSignature(implicit ctx: Context): Signature

    protected def resultSignature(implicit ctx: Context) = try resultType match {
      case rtp: MethodicType => rtp.signature
      case tp => Signature(tp, isJava = false)
    }
    catch {
      case ex: AssertionError =>
        println(i"failure while taking result signture of $this: $resultType")
        throw ex
    }

    final override def signature(implicit ctx: Context): Signature = {
      if (ctx.runId != mySignatureRunId) {
        mySignature = computeSignature
        mySignatureRunId = ctx.runId
      }
      mySignature
    }
  }

  trait MethodOrPoly extends MethodicType

  abstract case class MethodType(paramNames: List[TermName], paramTypes: List[Type])
      (resultTypeExp: MethodType => Type)
    extends CachedGroundType with BindingType with TermType with MethodOrPoly with NarrowCached { thisMethodType =>
    import MethodType._

    def isJava = false
    def isImplicit = false

    private[core] val resType = resultTypeExp(this)
    assert(resType.exists)

    override def resultType(implicit ctx: Context): Type =
      if (dependencyStatus == FalseDeps) { // dealias all false dependencies
        val dealiasMap = new TypeMap {
          def apply(tp: Type) = tp match {
            case tp @ TypeRef(pre, name) =>
              tp.info match {
                case TypeAlias(alias) if depStatus(pre) == TrueDeps => apply(alias)
                case _ => mapOver(tp)
              }
            case _ =>
              mapOver(tp)
          }
        }
        dealiasMap(resType)
      }
      else resType

    var myDependencyStatus: DependencyStatus = Unknown

    private def depStatus(tp: Type)(implicit ctx: Context): DependencyStatus = {
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
              case MethodParam(`thisMethodType`, _) => TrueDeps
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
      depStatusAcc(NoDeps, tp)
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
        val result = depStatus(resType)
        if ((result & Provisional) == 0) myDependencyStatus = result
        (result & StatusMask).toByte
      }
    }

    /** Does result type contain references to parameters of this method type,
     *  which cannot be eliminated by de-aliasing?
     */
    def isDependent(implicit ctx: Context): Boolean = dependencyStatus == TrueDeps

    protected def computeSignature(implicit ctx: Context): Signature =
      resultSignature.prepend(paramTypes, isJava)

    def derivedMethodType(paramNames: List[TermName], paramTypes: List[Type], resType: Type)(implicit ctx: Context) =
      if ((paramNames eq this.paramNames) && (paramTypes eq this.paramTypes) && (resType eq this.resType)) this
      else {
        val resTypeFn = (x: MethodType) => resType.subst(this, x)
        if (isJava) JavaMethodType(paramNames, paramTypes)(resTypeFn)
        else if (isImplicit) ImplicitMethodType(paramNames, paramTypes)(resTypeFn)
        else MethodType(paramNames, paramTypes)(resTypeFn)
      }

    def instantiate(argTypes: => List[Type])(implicit ctx: Context): Type =
      if (isDependent) resultType.substParams(this, argTypes)
      else resultType

    override def equals(that: Any) = that match {
      case that: MethodType =>
        this.paramNames == that.paramNames &&
        this.paramTypes == that.paramTypes &&
        this.resType == that.resType
      case _ =>
        false
    }

    override def computeHash = doHash(paramNames, resType, paramTypes)

    protected def prefixString = "MethodType"
    override def toString = s"$prefixString($paramNames, $paramTypes, $resType)"
  }

  final class CachedMethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)
    extends MethodType(paramNames, paramTypes)(resultTypeExp) {
    override def equals(that: Any) = super.equals(that) && that.isInstanceOf[CachedMethodType]
  }

  final class JavaMethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)
    extends MethodType(paramNames, paramTypes)(resultTypeExp) {
    override def isJava = true
    override def equals(that: Any) = super.equals(that) && that.isInstanceOf[JavaMethodType]
    override def computeHash = addDelta(super.computeHash, 1)
    override protected def prefixString = "JavaMethodType"
  }

  final class ImplicitMethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)
    extends MethodType(paramNames, paramTypes)(resultTypeExp) {
    override def isImplicit = true
    override def equals(that: Any) = super.equals(that) && that.isInstanceOf[ImplicitMethodType]
    override def computeHash = addDelta(super.computeHash, 2)
    override protected def prefixString = "ImplicitMethodType"
  }

  abstract class MethodTypeCompanion {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context): MethodType
    def apply(paramNames: List[TermName], paramTypes: List[Type], resultType: Type)(implicit ctx: Context): MethodType =
      apply(paramNames, paramTypes)(_ => resultType)
    def apply(paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context): MethodType =
      apply(nme.syntheticParamNames(paramTypes.length), paramTypes)(resultTypeExp)
    def apply(paramTypes: List[Type], resultType: Type)(implicit ctx: Context): MethodType =
      apply(nme.syntheticParamNames(paramTypes.length), paramTypes, resultType)
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
      def paramInfo(param: Symbol): Type = translateRepeated(param.info)
      def transformResult(mt: MethodType) =
        resultType.subst(params, (0 until params.length).toList map (MethodParam(mt, _)))
      apply(params map (_.name.asTermName), params map paramInfo)(transformResult _)
    }
  }

  object MethodType extends MethodTypeCompanion {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context) =
      unique(new CachedMethodType(paramNames, paramTypes)(resultTypeExp))

    private type DependencyStatus = Byte
    private final val Unknown: DependencyStatus = 0   // not yet computed
    private final val NoDeps: DependencyStatus = 1    // no dependent parameters found
    private final val FalseDeps: DependencyStatus = 2 // all dependent parameters are prefixes of non-depended alias types
    private final val TrueDeps: DependencyStatus = 3  // some truly dependent parameters exist
    private final val StatusMask: DependencyStatus = 3 // the bits indicating actual dependency status
    private final val Provisional: DependencyStatus = 4  // set if dependency status can still change due to type variable instantiations
  }

  object JavaMethodType extends MethodTypeCompanion {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context) =
      unique(new JavaMethodType(paramNames, paramTypes)(resultTypeExp))
  }

  object ImplicitMethodType extends MethodTypeCompanion {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context) =
      unique(new ImplicitMethodType(paramNames, paramTypes)(resultTypeExp))
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

  abstract case class PolyType(paramNames: List[TypeName])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type)
    extends CachedGroundType with BindingType with TermType with MethodOrPoly {

    val paramBounds = paramBoundsExp(this)
    val resType = resultTypeExp(this)

    assert(resType ne null)

    override def resultType(implicit ctx: Context) = resType

    protected def computeSignature(implicit ctx: Context) = resultSignature

    def isPolymorphicMethodType: Boolean = resType match {
      case _: MethodType => true
      case _ => false
    }

    def instantiate(argTypes: List[Type])(implicit ctx: Context): Type =
      resultType.substParams(this, argTypes)

    def instantiateBounds(argTypes: List[Type])(implicit ctx: Context): List[TypeBounds] =
      paramBounds.mapConserve(_.substParams(this, argTypes).bounds)

    def derivedPolyType(paramNames: List[TypeName], paramBounds: List[TypeBounds], resType: Type)(implicit ctx: Context) =
      if ((paramNames eq this.paramNames) && (paramBounds eq this.paramBounds) && (resType eq this.resType)) this
      else duplicate(paramNames, paramBounds, resType)

    def duplicate(paramNames: List[TypeName] = this.paramNames, paramBounds: List[TypeBounds] = this.paramBounds, resType: Type)(implicit ctx: Context) =
      PolyType(paramNames)(
          x => paramBounds mapConserve (_.subst(this, x).bounds),
          x => resType.subst(this, x))

    override def equals(other: Any) = other match {
      case other: PolyType =>
        other.paramNames == this.paramNames && other.paramBounds == this.paramBounds && other.resType == this.resType
      case _ => false
    }
    override def computeHash = {
      doHash(paramNames, resType, paramBounds)
    }

    override def toString = s"PolyType($paramNames, $paramBounds, $resType)"
  }

  class CachedPolyType(paramNames: List[TypeName])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type)
    extends PolyType(paramNames)(paramBoundsExp, resultTypeExp)

  object PolyType {
    def apply(paramNames: List[TypeName])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type)(implicit ctx: Context): PolyType = {
      unique(new CachedPolyType(paramNames)(paramBoundsExp, resultTypeExp))
    }

    def fromSymbols(tparams: List[Symbol], resultType: Type)(implicit ctx: Context) =
      if (tparams.isEmpty) resultType
      else {
        def transform(pt: PolyType, tp: Type) =
          tp.subst(tparams, (0 until tparams.length).toList map (PolyParam(pt, _)))
        apply(tparams map (_.name.asTypeName))(
          pt => tparams map (tparam => transform(pt, tparam.info).bounds),
          pt => transform(pt, resultType))
      }
  }

  // ----- Bound types: MethodParam, PolyParam, RefinedThis --------------------------

  abstract class BoundType extends CachedProxyType with ValueType {
    type BT <: Type
    def binder: BT
    // Dotty deviation: copyBoundType was copy, but
    // dotty generates copy methods always automatically, and therefore
    // does not accept same-named method definitions in subclasses.
    // Scala2x, on the other hand, requires them (not sure why!)
    def copyBoundType(bt: BT): Type
  }

  abstract class ParamType extends BoundType {
    def paramNum: Int
    def paramName: Name
  }

  abstract case class MethodParam(binder: MethodType, paramNum: Int) extends ParamType with SingletonType {
    type BT = MethodType

    def paramName = binder.paramNames(paramNum)

    override def underlying(implicit ctx: Context): Type = binder.paramTypes(paramNum)
    def copyBoundType(bt: BT) = new MethodParamImpl(bt, paramNum)

    // need to customize hashCode and equals to prevent infinite recursion for dep meth types.
    override def computeHash = addDelta(binder.identityHash, paramNum)
    override def equals(that: Any) = that match {
      case that: MethodParam =>
        (this.binder eq that.binder) && this.paramNum == that.paramNum
      case _ =>
        false
    }

    override def toString = s"MethodParam($paramName)"
  }

  class MethodParamImpl(binder: MethodType, paramNum: Int) extends MethodParam(binder, paramNum)

  object MethodParam {
    def apply(binder: MethodType, paramNum: Int)(implicit ctx: Context): MethodParam = {
      assertUnerased()
      new MethodParamImpl(binder, paramNum)
    }
  }

  /** TODO Some docs would be nice here! */
  case class PolyParam(binder: PolyType, paramNum: Int) extends ParamType {
    type BT = PolyType
    def copyBoundType(bt: BT) = PolyParam(bt, paramNum)

    /** Looking only at the structure of `bound`, is one of the following true?
     *     - fromBelow and param <:< bound
     *     - !fromBelow and param >:> bound
     */
    def occursIn(bound: Type, fromBelow: Boolean)(implicit ctx: Context): Boolean = bound.stripTypeVar match {
      case bound: PolyParam => bound == this
      case bound: AndOrType =>
        def occ1 = occursIn(bound.tp1, fromBelow)
        def occ2 = occursIn(bound.tp2, fromBelow)
        if (fromBelow == bound.isAnd) occ1 && occ2 else occ1 || occ2
      case _ => false
    }

    def paramName = binder.paramNames(paramNum)

    override def underlying(implicit ctx: Context): Type = binder.paramBounds(paramNum)
    // no customized hashCode/equals needed because cycle is broken in PolyType
    override def toString = s"PolyParam($paramName)"

    override def computeHash = doHash(paramNum, binder.identityHash)

    override def equals(that: Any) = that match {
      case that: PolyParam =>
        (this.binder eq that.binder) && this.paramNum == that.paramNum
      case _ =>
        false
    }
  }

  /** a this-reference to an enclosing refined type `binder`. */
  case class RefinedThis(binder: RefinedType) extends BoundType with SingletonType {
    type BT = RefinedType
    override def underlying(implicit ctx: Context) = binder
    def copyBoundType(bt: BT) = RefinedThis(bt)

    // need to customize hashCode and equals to prevent infinite recursion for
    // refinements that refer to the refinement type via this
    override def computeHash = addDelta(binder.identityHash, 41)
    override def equals(that: Any) = that match {
      case that: RefinedThis => this.binder eq that.binder
      case _ => false
    }
    override def toString = s"RefinedThis(${binder.hashCode})"
  }

  // ----- Skolem types -----------------------------------------------

  /** A skolem type reference with underlying type `binder`. */
  abstract case class SkolemType(info: Type) extends UncachedProxyType with ValueType with SingletonType {
    override def underlying(implicit ctx: Context) = info
    def derivedSkolemType(info: Type)(implicit ctx: Context) =
      if (info eq this.info) this else SkolemType(info)
    override def hashCode: Int = identityHash
    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
    override def toString = s"Skolem($info)"
  }

  final class CachedSkolemType(info: Type) extends SkolemType(info)

  object SkolemType {
    def apply(info: Type)(implicit ctx: Context) =
      unique(new CachedSkolemType(info))
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
   *  @param  owningTree    The function part of the TypeApply tree tree that introduces
   *                        the type variable.
   *  @paran  owner         The current owner if the context where the variable was created.
   *
   *  `owningTree` and `owner` are used to determine whether a type-variable can be instantiated
   *  at some given point. See `Inferencing#interpolateUndetVars`.
   */
  final class TypeVar(val origin: PolyParam, creatorState: TyperState, val owningTree: untpd.Tree, val owner: Symbol) extends CachedProxyType with ValueType {

    /** The permanent instance type of the variable, or NoType is none is given yet */
    private[core] var inst: Type = NoType

    /** The state owning the variable. This is at first `creatorState`, but it can
     *  be changed to an enclosing state on a commit.
     */
    private[core] var owningState = creatorState

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
      assert(ctx.typerState.constraint contains this) // !!! DEBUG
      if ((ctx.typerState eq owningState) && !ctx.typeComparer.subtypeCheckInProgress)
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
    def instantiate(fromBelow: Boolean)(implicit ctx: Context): Type = {
      def upperBound = ctx.typerState.constraint.fullUpperBound(origin)
      def isSingleton(tp: Type): Boolean = tp match {
        case tp: SingletonType => true
        case AndType(tp1, tp2) => isSingleton(tp1) | isSingleton(tp2)
        case OrType(tp1, tp2) => isSingleton(tp1) & isSingleton(tp2)
        case _ => false
      }
      def isFullyDefined(tp: Type): Boolean = tp match {
        case tp: TypeVar => tp.isInstantiated && isFullyDefined(tp.instanceOpt)
        case tp: TypeProxy => isFullyDefined(tp.underlying)
        case tp: AndOrType => isFullyDefined(tp.tp1) && isFullyDefined(tp.tp2)
        case _ => true
      }
      def isOrType(tp: Type): Boolean = tp.stripTypeVar.dealias match {
        case tp: OrType => true
        case AndType(tp1, tp2) => isOrType(tp1) | isOrType(tp2)
        case RefinedType(parent, _) => isOrType(parent)
        case WildcardType(bounds: TypeBounds) => isOrType(bounds.hi)
        case _ => false
      }

      // First, solve the constraint.
      var inst = ctx.typeComparer.approximation(origin, fromBelow)

      // Then, approximate by (1.) - (3.) and simplify as follows.
      // 1. If instance is from below and is a singleton type, yet
      // upper bound is not a singleton type, widen the instance.
      if (fromBelow && isSingleton(inst) && !isSingleton(upperBound))
        inst = inst.widen

      inst = inst.simplified

      // 2. If instance is from below and is a fully-defined union type, yet upper bound
      // is not a union type, approximate the union type from above by an intersection
      // of all common base types.
      if (fromBelow && isOrType(inst) && isFullyDefined(inst) && !isOrType(upperBound))
        inst = inst.approximateUnion

      // 3. If instance is from below, and upper bound has open named parameters
      //    make sure the instance has all named parameters of the bound.
      if (fromBelow) inst = inst.widenToNamedTypeParams(this.namedTypeParams)

      if (ctx.typerState.isGlobalCommittable)
        assert(!inst.isInstanceOf[PolyParam], i"bad inst $this := $inst, constr = ${ctx.typerState.constraint}")
          // If this fails, you might want to turn on Config.debugCheckConstraintsClosed
          // to help find the root of the problem.

      instantiateWith(inst)
    }

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
      classParents: List[TypeRef],
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
          val given = givenSelfType
          val raw =
            if (!given.exists) fullRef
            else if (cls is Module) given
            else if (ctx.erasedTypes) fullRef
            else AndType(given, fullRef)
          raw//.asSeenFrom(prefix, cls.owner)
        }
      selfTypeCache
    }

    /** The explicitly given self type (self types of modules are assumed to be
     *  explcitly given here).
     */
    override def givenSelfType(implicit ctx: Context): Type = selfInfo match {
      case tp: Type => tp
      case self: Symbol => self.info
    }

    private var selfTypeCache: Type = null

    private def fullyAppliedRef(base: Type, tparams: List[TypeSymbol])(implicit ctx: Context): Type = tparams match {
      case tparam :: tparams1 =>
        fullyAppliedRef(
          RefinedType(base, tparam.name, TypeRef(cls.thisType, tparam).toBounds(tparam)),
          tparams1)
      case nil =>
        base
    }

    /** The class type with all type parameters */
    def fullyAppliedRef(implicit ctx: Context): Type = fullyAppliedRef(cls.typeRef, cls.typeParams)

    def rebase(tp: Type)(implicit ctx: Context): Type =
      if ((prefix eq cls.owner.thisType) || !cls.owner.isClass || ctx.erasedTypes) tp
      else tp.substThis(cls.owner.asClass, prefix)

    private var typeRefCache: TypeRef = null

    def typeRef(implicit ctx: Context): TypeRef = {
      def clsDenot = if (prefix eq cls.owner.thisType) cls.denot else cls.denot.copySymDenotation(info = this)
      if (typeRefCache == null)
        typeRefCache =
          if ((cls is PackageClass) || cls.owner.isTerm) symbolicTypeRef
          else TypeRef(prefix, cls.name, clsDenot)
      typeRefCache
    }

    def symbolicTypeRef(implicit ctx: Context): TypeRef = TypeRef(prefix, cls)

    // cached because baseType needs parents
    private var parentsCache: List[TypeRef] = null

    /** The parent type refs as seen from the given prefix */
    override def parents(implicit ctx: Context): List[TypeRef] = {
      if (parentsCache == null)
        parentsCache = cls.classParents.mapConserve(rebase(_).asInstanceOf[TypeRef])
      parentsCache
    }

    /** The parent types with all type arguments */
    override def parentsWithArgs(implicit ctx: Context): List[Type] =
      parents mapConserve { pref =>
        ((pref: Type) /: pref.classSymbol.typeParams) { (parent, tparam) =>
          val targSym = decls.lookup(tparam.name)
          if (targSym.exists) RefinedType(parent, targSym.name, targSym.info)
          else parent
        }
      }

    def derivedClassInfo(prefix: Type)(implicit ctx: Context) =
      if (prefix eq this.prefix) this
      else ClassInfo(prefix, cls, classParents, decls, selfInfo)

    def derivedClassInfo(prefix: Type = this.prefix, classParents: List[TypeRef] = classParents, decls: Scope = this.decls, selfInfo: DotClass = this.selfInfo)(implicit ctx: Context) =
      if ((prefix eq this.prefix) && (classParents eq this.classParents) && (decls eq this.decls) && (selfInfo eq this.selfInfo)) this
      else ClassInfo(prefix, cls, classParents, decls, selfInfo)

    override def computeHash = doHash(cls, prefix)

    override def toString = s"ClassInfo($prefix, $cls)"
  }

  final class CachedClassInfo(prefix: Type, cls: ClassSymbol, classParents: List[TypeRef], decls: Scope, selfInfo: DotClass)
    extends ClassInfo(prefix, cls, classParents, decls, selfInfo)

  object ClassInfo {
    def apply(prefix: Type, cls: ClassSymbol, classParents: List[TypeRef], decls: Scope, selfInfo: DotClass = NoType)(implicit ctx: Context) =
      unique(new CachedClassInfo(prefix, cls, classParents, decls, selfInfo))
  }

  /** Type bounds >: lo <: hi
   *  @param bindingKind: If != NoBinding, it indicates that this is
   *                      an introduction of a higher-kinded type parameter.
   *                      In that case it also defines the variance of the parameter.
   */
  abstract case class TypeBounds(lo: Type, hi: Type)(val bindingKind: BindingKind) extends CachedProxyType with TypeType {

    assert(lo.isInstanceOf[TermType])
    assert(hi.isInstanceOf[TermType])

    def variance: Int = 0

    override def underlying(implicit ctx: Context): Type = hi

    /** The non-alias type bounds type with given bounds */
    def derivedTypeBounds(lo: Type, hi: Type, bk: BindingKind = this.bindingKind)(implicit ctx: Context) =
      if ((lo eq this.lo) && (hi eq this.hi) && (bk == this.bindingKind) && (variance == 0)) this
      else TypeBounds(lo, hi, bk)

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
      else TypeBounds(this.lo | that.lo, this.hi & that.hi, this.bindingKind join that.bindingKind)

    def | (that: TypeBounds)(implicit ctx: Context): TypeBounds =
      if ((this.lo frozen_<:< that.lo) && (that.hi frozen_<:< this.hi)) this
      else if ((that.lo frozen_<:< this.lo) && (this.hi frozen_<:< that.hi)) that
      else TypeBounds(this.lo & that.lo, this.hi | that.hi, this.bindingKind join that.bindingKind)

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
        (this.lo eq that.lo) && (this.hi eq that.hi) && this.variance == that.variance
      case _ =>
        false
    }

    override def toString =
      if (lo eq hi) s"TypeAlias($lo, $variance)" else s"TypeBounds($lo, $hi)"
  }

  class RealTypeBounds(lo: Type, hi: Type, bk: BindingKind) extends TypeBounds(lo, hi)(bk)

  abstract class TypeAlias(val alias: Type, override val variance: Int) extends TypeBounds(alias, alias)(NoBinding) {
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
    def apply(lo: Type, hi: Type, bk: BindingKind = NoBinding)(implicit ctx: Context): TypeBounds =
      unique(new RealTypeBounds(lo, hi, bk))
    def empty(implicit ctx: Context) = apply(defn.NothingType, defn.AnyType)
    def upper(hi: Type)(implicit ctx: Context) = apply(defn.NothingType, hi)
    def lower(lo: Type)(implicit ctx: Context) = apply(lo, defn.AnyType)
  }

  object TypeAlias {
    def apply(alias: Type, variance: Int = 0)(implicit ctx: Context) =
      ctx.uniqueTypeAliases.enterIfNew(alias, variance)
    def unapply(tp: TypeAlias): Option[Type] = Some(tp.alias)
  }

  /** A value class defining the interpretation of a TypeBounds
   *  as either a regular type bounds or a binding (i.e. introduction) of a
   *  higher-kinded type parameter.
   */
  class BindingKind(val n: Byte) extends AnyVal {
    def join(that: BindingKind) =
      if (this == that) this
      else if (this == NoBinding) that
      else if (that == NoBinding) this
      else NonvariantBinding
  }

  val NoBinding = new BindingKind(0)             // Regular type bounds
  val ContravariantBinding = new BindingKind(1)  // Bounds for contravariant hk type param
  val NonvariantBinding = new BindingKind(2)     // Bounds for nonvariant hk type param
  val CovariantBinding = new BindingKind(3)      // Bounds for covariant hk type param

  object BindingKind {
    def fromVariance(v: Int): BindingKind = new BindingKind((v + NonvariantBinding.n).toByte)
    def toVariance(bk: BindingKind): Int = bk.n
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

  abstract class ErrorType extends UncachedGroundType with ValueType

  object ErrorType extends ErrorType

  /** Wildcard type, possibly with bounds */
  abstract case class WildcardType(optBounds: Type) extends CachedGroundType with TermType {
    def derivedWildcardType(optBounds: Type)(implicit ctx: Context) =
      if (optBounds eq this.optBounds) this else WildcardType(optBounds.asInstanceOf[TypeBounds])
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
        def zeroParams(tp: Type): Boolean = tp match {
          case pt: PolyType => zeroParams(pt.resultType)
          case mt: MethodType => mt.paramTypes.isEmpty && !mt.resultType.isInstanceOf[MethodType]
          case et: ExprType => true
          case _ => false
        }
        if ((tp.cls is Trait) || zeroParams(tp.cls.primaryConstructor.info)) tp // !!! needs to be adapted once traits have parameters
        else NoType
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

  abstract class TypeMap(implicit protected val ctx: Context) extends (Type => Type) { thisMap =>

    protected def stopAtStatic = true

    def apply(tp: Type): Type

    protected var variance = 1

    protected def derivedSelect(tp: NamedType, pre: Type): Type =
      tp.derivedSelect(pre)
    protected def derivedRefinedType(tp: RefinedType, parent: Type, info: Type): Type =
      tp.derivedRefinedType(parent, tp.refinedName, info)
    protected def derivedTypeAlias(tp: TypeAlias, alias: Type): Type =
      tp.derivedTypeAlias(alias)
    protected def derivedTypeBounds(tp: TypeBounds, lo: Type, hi: Type): Type =
      tp.derivedTypeBounds(lo, hi)
    protected def derivedSuperType(tp: SuperType, thistp: Type, supertp: Type): Type =
      tp.derivedSuperType(thistp, supertp)
    protected def derivedAndOrType(tp: AndOrType, tp1: Type, tp2: Type): Type =
      tp.derivedAndOrType(tp1, tp2)
    protected def derivedSkolemType(tp: SkolemType, info: Type): Type =
      tp.derivedSkolemType(info)
    protected def derivedAnnotatedType(tp: AnnotatedType, underlying: Type, annot: Annotation): Type =
      tp.derivedAnnotatedType(underlying, annot)
    protected def derivedWildcardType(tp: WildcardType, bounds: Type): Type =
      tp.derivedWildcardType(bounds)
    protected def derivedClassInfo(tp: ClassInfo, pre: Type): Type =
      tp.derivedClassInfo(pre)
    protected def derivedJavaArrayType(tp: JavaArrayType, elemtp: Type): Type =
      tp.derivedJavaArrayType(elemtp)
    protected def derivedMethodType(tp: MethodType, formals: List[Type], restpe: Type): Type =
      tp.derivedMethodType(tp.paramNames, formals, restpe)
    protected def derivedExprType(tp: ExprType, restpe: Type): Type =
      tp.derivedExprType(restpe)
    protected def derivedPolyType(tp: PolyType, pbounds: List[TypeBounds], restpe: Type): Type =
      tp.derivedPolyType(tp.paramNames, pbounds, restpe)

    /** Map this function over given type */
    def mapOver(tp: Type): Type = {
      implicit val ctx: Context = this.ctx // Dotty deviation: implicits need explicit type
      tp match {
        case tp: NamedType =>
          if (stopAtStatic && tp.symbol.isStatic) tp
          else derivedSelect(tp, this(tp.prefix))

        case _: ThisType
          | _: BoundType
          | NoPrefix => tp

        case tp: RefinedType =>
          derivedRefinedType(tp, this(tp.parent), this(tp.refinedInfo))

        case tp: TypeAlias =>
          val saved = variance
          variance = variance * tp.variance
          val alias1 = this(tp.alias)
          variance = saved
          derivedTypeAlias(tp, alias1)

        case tp: TypeBounds =>
          variance = -variance
          val lo1 = this(tp.lo)
          variance = -variance
          derivedTypeBounds(tp, lo1, this(tp.hi))

        case tp: MethodType =>
          def mapOverMethod = {
            variance = -variance
            val ptypes1 = tp.paramTypes mapConserve this
            variance = -variance
            derivedMethodType(tp, ptypes1, this(tp.resultType))
          }
          mapOverMethod

        case tp: ExprType =>
          derivedExprType(tp, this(tp.resultType))

        case tp: PolyType =>
          def mapOverPoly = {
            variance = -variance
            val bounds1 = tp.paramBounds.mapConserve(this).asInstanceOf[List[TypeBounds]]
            variance = -variance
            derivedPolyType(tp, bounds1, this(tp.resultType))
          }
          mapOverPoly

        case tp @ SuperType(thistp, supertp) =>
          derivedSuperType(tp, this(thistp), this(supertp))

        case tp: LazyRef =>
          LazyRef(() => this(tp.ref))

        case tp: ClassInfo =>
          mapClassInfo(tp)

        case tp: TypeVar =>
          val inst = tp.instanceOpt
          if (inst.exists) apply(inst) else tp

        case tp: AndOrType =>
          derivedAndOrType(tp, this(tp.tp1), this(tp.tp2))

        case tp: SkolemType =>
          derivedSkolemType(tp, this(tp.info))

        case tp @ AnnotatedType(underlying, annot) =>
          val underlying1 = this(underlying)
          if (underlying1 eq underlying) tp
          else derivedAnnotatedType(tp, underlying1, mapOver(annot))

        case tp @ WildcardType =>
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
      val parents1 = (tp.parents mapConserve this).asInstanceOf[List[TypeRef]]
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

  abstract class ApproximatingTypeMap(implicit ctx: Context) extends TypeMap { thisMap =>
    def approx(lo: Type = defn.NothingType, hi: Type = defn.AnyType) =
      if (variance == 0) NoType
      else apply(if (variance < 0) lo else hi)

    override protected def derivedSelect(tp: NamedType, pre: Type) =
      if (pre eq tp.prefix) tp
      else tp.info match {
        case TypeAlias(alias) => apply(alias) // try to heal by following aliases
        case _ =>
          if (pre.exists && !pre.isRef(defn.NothingClass) && variance > 0) tp.derivedSelect(pre)
          else tp.info match {
            case TypeBounds(lo, hi) => approx(lo, hi)
            case _ => approx()
          }
      }
    override protected def derivedRefinedType(tp: RefinedType, parent: Type, info: Type) =
      if (parent.exists && info.exists) tp.derivedRefinedType(parent, tp.refinedName, info)
      else approx(hi = parent)
    override protected def derivedTypeAlias(tp: TypeAlias, alias: Type) =
      if (alias.exists) tp.derivedTypeAlias(alias)
      else approx(NoType, TypeBounds.empty)
    override protected def derivedTypeBounds(tp: TypeBounds, lo: Type, hi: Type) =
      if (lo.exists && hi.exists) tp.derivedTypeBounds(lo, hi)
      else approx(NoType,
        if (lo.exists) TypeBounds.lower(lo)
        else if (hi.exists) TypeBounds.upper(hi)
        else TypeBounds.empty)
    override protected def derivedSuperType(tp: SuperType, thistp: Type, supertp: Type) =
      if (thistp.exists && supertp.exists) tp.derivedSuperType(thistp, supertp)
      else NoType
    override protected def derivedAndOrType(tp: AndOrType, tp1: Type, tp2: Type) =
      if (tp1.exists && tp2.exists) tp.derivedAndOrType(tp1, tp2)
      else if (tp.isAnd) approx(hi = tp1 & tp2)  // if one of tp1d, tp2d exists, it is the result of tp1d & tp2d
      else approx(lo = tp1 & tp2)
    override protected def derivedSkolemType(tp: SkolemType, info: Type) =
      if (info.exists) tp.derivedSkolemType(info)
      else NoType
    override protected def derivedAnnotatedType(tp: AnnotatedType, underlying: Type, annot: Annotation) =
      if (underlying.exists) tp.derivedAnnotatedType(underlying, annot)
      else NoType
    override protected def derivedWildcardType(tp: WildcardType, bounds: Type) =
      if (bounds.exists) tp.derivedWildcardType(bounds)
      else WildcardType
    override protected def derivedClassInfo(tp: ClassInfo, pre: Type): Type =
      if (pre.exists) tp.derivedClassInfo(pre)
      else NoType
  }

  // ----- TypeAccumulators ----------------------------------------------------

  abstract class TypeAccumulator[T](implicit protected val ctx: Context) extends ((T, Type) => T) {

    protected def stopAtStatic = true

    def apply(x: T, tp: Type): T

    protected def applyToAnnot(x: T, annot: Annotation): T = x // don't go into annotations

    protected var variance = 1

    protected def applyToPrefix(x: T, tp: NamedType) = {
      val saved = variance
      variance = 0
      val result = this(x, tp.prefix)
      variance = saved
      result
    }

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

      case tp: RefinedType =>
        this(this(x, tp.parent), tp.refinedInfo)

      case bounds @ TypeBounds(lo, hi) =>
        if (lo eq hi) {
          val saved = variance
          variance = variance * bounds.variance
          val result = this(x, lo)
          variance = saved
          result
        }
        else {
          variance = -variance
          val y = this(x, lo)
          variance = -variance
          this(y, hi)
        }

      case tp @ MethodType(pnames, ptypes) =>
        variance = -variance
        val y = foldOver(x, ptypes)
        variance = -variance
        this(y, tp.resultType)

      case ExprType(restpe) =>
        this(x, restpe)

      case tp @ PolyType(pnames) =>
        variance = -variance
        val y = foldOver(x, tp.paramBounds)
        variance = -variance
        this(y, tp.resultType)

      case SuperType(thistp, supertp) =>
        this(this(x, thistp), supertp)

      case tp @ ClassInfo(prefix, _, _, _, _) =>
        this(x, prefix)

      case tp: AndOrType =>
        this(this(x, tp.tp1), tp.tp2)

      case tp: SkolemType =>
        this(x, tp.info)

      case AnnotatedType(underlying, annot) =>
        this(applyToAnnot(x, annot), underlying)

      case tp: TypeVar =>
        this(x, tp.underlying)

      case tp: WildcardType =>
        this(x, tp.optBounds)

      case tp: JavaArrayType =>
        this(x, tp.elemType)

      case tp: ProtoType =>
        tp.fold(x, this)

      case _ => x
    }

    final def foldOver(x: T, ts: List[Type]): T = ts match {
      case t :: ts1 => foldOver(apply(x, t), ts1)
      case nil => x
    }
  }

  abstract class TypeTraverser(implicit ctx: Context) extends TypeAccumulator[Unit] {
    def traverse(tp: Type): Unit
    def apply(x: Unit, tp: Type): Unit = traverse(tp)
    protected def traverseChildren(tp: Type) = foldOver((), tp)
  }

  class ExistsAccumulator(p: Type => Boolean)(implicit ctx: Context) extends TypeAccumulator[Boolean] {
    override def stopAtStatic = false
    def apply(x: Boolean, tp: Type) = x || p(tp) || foldOver(x, tp)
  }

  class ForeachAccumulator(p: Type => Unit)(implicit ctx: Context) extends TypeAccumulator[Unit] {
    override def stopAtStatic = false
    def apply(x: Unit, tp: Type): Unit = foldOver(p(tp), tp)
  }

  class HasUnsafeNonAccumulator(implicit ctx: Context) extends TypeAccumulator[Boolean] {
    def apply(x: Boolean, tp: Type) = x || tp.isUnsafeNonvariant || foldOver(x, tp)
  }

  class NamedPartsAccumulator(p: NamedType => Boolean)(implicit ctx: Context) extends TypeAccumulator[mutable.Set[NamedType]] {
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
          case TypeBounds(_, hi) =>
            apply(x, hi)
          case tp: ThisType =>
            apply(x, tp.tref)
          case tp: ConstantType =>
            apply(x, tp.underlying)
          case tp: MethodParam =>
            apply(x, tp.underlying)
          case tp: PolyParam =>
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
        val mbr = pre.member(name)
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
      name.isTermName && (pre member name).hasAltWith(_.symbol is Deferred)
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
       |the classfile defining the type might be missing from the classpath${otherReason(pre)}""".stripMargin) {
    if (ctx.debug) printStackTrace()
  }

  private def otherReason(pre: Type)(implicit ctx: Context): String = pre match {
    case pre: ThisType if pre.givenSelfType.exists =>
      i"\nor the self type of $pre might not contain all transitive dependencies"
    case _ => ""
  }

  class CyclicReference private (val denot: SymDenotation)
    extends TypeError(s"cyclic reference involving $denot") {
    def show(implicit ctx: Context) = s"cyclic reference involving ${denot.show}"
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
