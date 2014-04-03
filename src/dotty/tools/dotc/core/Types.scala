package dotty.tools.dotc
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
import ast.tpd._, printing.Texts._
import ast.untpd
import transform.Erasure
import printing.Printer
import Hashable._
import Uniques._
import collection.{mutable, Seq, breakOut}
import config.Config
import config.Printers._
import annotation.tailrec
import language.implicitConversions

object Types {

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
   *        |              |                +--- RefinedThis
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

    /** Is this type different from NoType? */
    def exists: Boolean = true

    /** This type, if it exists, otherwise `that` type */
    def orElse(that: => Type) = if (exists) this else that

    /** Is this type a value type? */
    final def isValueType: Boolean = this.isInstanceOf[ValueType]

    /** Does this type denote a stable reference (i.e. singleton type)? */
    final def isStable(implicit ctx: Context): Boolean = this match {
      case tp: TermRef => tp.termSymbol.isStable
      case _: SingletonType => true
      case NoPrefix => true
      case _ => false
    }

    /** Is this type a (possibly aliased and/or partially applied) type reference
     *  to the given type symbol?
     *  @sym  The symbol to compare to. It must be a class symbol or abstract type.
     *        It makes no sense for it to be an alias type because isRef would always
     *        return false in that case.
     */
    def isRef(sym: Symbol)(implicit ctx: Context): Boolean = stripTypeVar match {
      case this1: TypeRef =>
        val thissym = this1.symbol
        if (thissym.isAliasType) this1.info.bounds.hi.isRef(sym)
        else thissym eq sym
      case this1: RefinedType =>
        // make sure all refinements are type arguments
        this1.parent.isRef(sym) && this.argInfos.nonEmpty
      case _ =>
        false
    }

    /** Is this type an instance of a non-bottom subclass of the given class `cls`? */
    final def derivesFrom(cls: Symbol)(implicit ctx: Context): Boolean = this match {
      case tp: TypeRef =>
        val sym = tp.symbol
        if (sym.isClass) sym.derivesFrom(cls) else tp.underlying.derivesFrom(cls)
      case tp: TypeProxy =>
        tp.underlying.derivesFrom(cls)
      case tp: AndType =>
        tp.tp1.derivesFrom(cls) || tp.tp2.derivesFrom(cls)
      case tp: OrType =>
        tp.tp1.derivesFrom(cls) && tp.tp2.derivesFrom(cls)
      case _ =>
        false
    }

   /** A type T is a legal prefix in a type selection T#A if
     *  T is stable or T contains no abstract types
     *  !!! Todo: What about non-final vals that contain abstract types?
     */
    final def isLegalPrefix(implicit ctx: Context): Boolean =
      isStable || {
        val absTypeNames = memberNames(abstractTypeNameFilter)
        if (absTypeNames.nonEmpty) typr.println(s"abstract type members of ${this.showWithUnderlying()}: $absTypeNames")
        absTypeNames.isEmpty
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
      case AnnotatedType(annot, tp) => (annot matches cls) || (tp hasAnnotation cls)
      case _ => false
    }

    /** Does this type occur as a part of type `that`? */
    final def occursIn(that: Type)(implicit ctx: Context): Boolean =
      that existsPart (this == _)

    /** Is this a type of a repeated parameter? */
    def isRepeatedParam(implicit ctx: Context): Boolean =
      defn.RepeatedParamClasses contains typeSymbol

    /** Is this an alias TypeBounds? */
    def isAlias: Boolean = this match {
      case TypeBounds(lo, hi) => lo eq hi
      case _ => false
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

    /** The parts of this type which are type or term refs */
    final def namedParts(implicit ctx: Context): collection.Set[NamedType] =
      namedPartsWith(alwaysTrue)

    /** The parts of this type which are type or term refs and which
     *  satisfy predicate `p`.
     */
    def namedPartsWith(p: NamedType => Boolean)(implicit ctx: Context): collection.Set[NamedType] =
      new NamedPartsAccumulator(p).apply(mutable.LinkedHashSet(), this)

    // needed?
    //final def foreach(f: Type => Unit): Unit = ???

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
      case OrType(l, r) =>
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
        l.classSymbols intersect r.classSymbols
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
      findMember(name, widenIfUnstable, EmptyFlags)
    }

    /** The non-private member of this type with the given name. */
    final def nonPrivateMember(name: Name)(implicit ctx: Context): Denotation = track("nonPrivateMember") {
      findMember(name, widenIfUnstable, Flags.Private)
    }

    /** Find member of this type with given name and
     *  produce a denotation that contains the type of the member
     *  as seen from given prefix `pre`. Exclude all members that have
     *  flags in `excluded` from consideration.
     */
    final def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Denotation = try {
      @tailrec def go(tp: Type): Denotation = tp match {
        case tp: RefinedType =>
          if (name eq tp.refinedName) goRefined(tp) else go(tp.parent)
        case tp: ThisType =>
          goThis(tp)
        case tp: TypeRef =>
          tp.denot.findMember(name, pre, excluded)
        case tp: TypeProxy =>
          go(tp.underlying)
        case tp: ClassInfo =>
          tp.cls.findMember(name, pre, excluded)
        case AndType(l, r) =>
          goAnd(l, r)
        case OrType(l, r) =>
          goOr(l, r)
        case ErrorType =>
          ctx.newErrorSymbol(pre.classSymbol orElse defn.RootClass, name)
        case _ =>
          NoDenotation
      }
      def goRefined(tp: RefinedType) = {
        val pdenot = go(tp.parent)
        val rinfo = tp.refinedInfo.substThis(tp, pre)
        if (name.isTypeName) // simplified case that runs more efficiently
          pdenot.asSingleDenotation.derivedSingleDenotation(pdenot.symbol, rinfo)
        else
          pdenot & (new JointRefDenotation(NoSymbol, rinfo, Period.allInRun(ctx.runId)), pre)
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
      def goAnd(l: Type, r: Type) = go(l) & (go(r), pre)
      def goOr(l: Type, r: Type) = go(l) | (go(r), pre)
      go(this)
    } catch {
      case ex: MergeError =>
        throw new MergeError(s"${ex.getMessage} as members of type ${pre.show}")
      case ex: Throwable =>
        println(s"error occurred during: $this: ${this.widen} member $name")
        throw ex // DEBUG
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

    private def memberDenots(keepOnly: NameFilter, f: (Name, mutable.Buffer[SingleDenotation]) => Unit)(implicit ctx: Context): Seq[SingleDenotation] = {
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

    /** The set of type members of this type */
    final def typeMembers(implicit ctx: Context): Seq[SingleDenotation] = track("typeMembers") {
      memberDenots(typeNameFilter,
          (name, buf) => buf += member(name).asSingleDenotation)
    }

    /** The set of implicit members of this type */
    final def implicitMembers(implicit ctx: Context): List[TermRef] = track("implicitMembers") {
      memberDenots(implicitFilter,
          (name, buf) => buf ++= member(name).altsWith(_ is Implicit))
        .toList.map(_.termRefWithSig)
    }

    /** The info of `sym`, seen as a member of this type. */
    final def memberInfo(sym: Symbol)(implicit ctx: Context): Type =
      sym.info.asSeenFrom(this, sym.owner)

    /** This type seen as if it were the type of a member of prefix type `pre`
     *  declared in class `cls`.
     */
    final def asSeenFrom(pre: Type, cls: Symbol)(implicit ctx: Context): Type = track("asSeenFrom") {
      if (!cls.membersNeedAsSeenFrom(pre)) this
      else ctx.asSeenFrom(this, pre, cls, null)
    }

// ----- Subtype-related --------------------------------------------

    /** Is this type a subtype of that type? */
    final def <:<(that: Type)(implicit ctx: Context): Boolean = track("<:<") {
      ctx.typeComparer.topLevelSubType(this, that)
    }

    /** Is this type the same as that type?
     *  This is the case iff `this <:< that` and `that <:< this`.
     */
    final def =:=(that: Type)(implicit ctx: Context): Boolean = track("=:=") {
      ctx.typeComparer.isSameType(this, that)
    }

    /** Is this type close enough to that type so that members
     *  with the two type would override each other?
     *  This means:
     *    - Either both types are polytypes with the same number of
     *      type parameters and their result types match after renaming
     *      corresponding type parameters
     *    - Or both types are (possibly nullary) method types with equivalent parameter types
     *      and matching result types
     *    - Or both types are equivalent
     *    - Or phase.erasedTypes is false and both types are neither method nor
     *      poly types.
     */
    def matches(that: Type)(implicit ctx: Context): Boolean =
      if (Config.newMatch) this.signature matches that.signature
      else track("matches") {
        ctx.typeComparer.matchesType(
          this, that, alwaysMatchSimple = !ctx.phase.erasedTypes)
      }

    /** The basetype TypeRef of this type with given class symbol,
     *  but without including any type arguments
     */
    final def baseTypeRef(base: Symbol)(implicit ctx: Context): Type = /*ctx.traceIndented(s"$this baseTypeRef $base")*/ /*>|>*/ track("baseTypeRef") /*<|<*/ {
      base.denot match {
        case classd: ClassDenotation => classd.baseTypeRefOf(this)//widen.dealias)
        case _ => NoType
      }
    }

    def & (that: Type)(implicit ctx: Context): Type = track("&") {
      ctx.typeComparer.glb(this, that)
    }

    def | (that: Type)(implicit ctx: Context): Type = track("|") {
      ctx.typeComparer.lub(this, that)
    }

// ----- Unwrapping types -----------------------------------------------

    /** Map a TypeVar to either its instance if it is instantiated, or its origin,
     *  if not, until the result is no longer a TypeVar. Identity on all other types.
     */
    def stripTypeVar(implicit ctx: Context): Type = this

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
     *  base type by applying one or more `underlying` dereferences,
     */
    final def widenSingleton(implicit ctx: Context): Type = this match {
      case tp: SingletonType if !tp.isOverloaded => tp.underlying.widenSingleton
      case _ => this
    }

    /** Widen from ExprType type to its result type.
     */
    final def widenExpr: Type = this match {
      case tp: ExprType => tp.resultType
      case _ => this
    }

    /** Widen type if it is unstable (i.e. an EpxprType, or Termref to unstable symbol */
    final def widenIfUnstable(implicit ctx: Context): Type = this match {
      case tp: ExprType => tp.resultType.widenIfUnstable
      case tp: TermRef if !tp.symbol.isStable => tp.underlying.widenIfUnstable
      case _ => this
    }

    /** Follow aliases until type is no longer an alias type. */
    final def dealias(implicit ctx: Context): Type = this match {
      case tp: TypeRef =>
        tp.info match {
          case TypeBounds(lo, hi) if lo eq hi => hi.dealias
          case _ => tp
        }
      case tp: TypeVar =>
        val tp1 = tp.instanceOpt
        if (tp1.exists) tp1.dealias else tp
      case tp => tp
    }

    /** Widen from constant type to its underlying non-constant
     *  base type.
     */
    final def deconst(implicit ctx: Context): Type = this match {
      case tp: ConstantType => tp.value.tpe
      case _ => this
    }

    /** If this is a refinement type, the unrefined parent,
     *  else the type itself.
     */
    final def unrefine(implicit ctx: Context): Type = stripTypeVar match {
      case tp @ RefinedType(tycon, _) => tycon.unrefine
      case _ => this
    }

    /** Map references to Object to references to Any; needed for Java interop */
    final def objToAny(implicit ctx: Context) =
      if ((this isRef defn.ObjectClass) && !ctx.phase.erasedTypes) defn.AnyType else this

    /** If this is repeated parameter type, its underlying type,
     *  else the type itself.
     */
    def underlyingIfRepeated(implicit ctx: Context): Type = this match {
      case rt @ RefinedType(tref: TypeRef, name) if defn.RepeatedParamClasses contains tref.symbol =>
        RefinedType(defn.SeqClass.typeRef, name, rt.refinedInfo)
      case _ =>
        this
    }

    /** If this is a (possibly aliased, annotated, and/or parameterized) reference to
     *  a class, the class type ref, otherwise NoType.
     */
    def underlyingClassRef(implicit ctx: Context): Type = dealias match {
      case tp: TypeRef if tp.symbol.isClass => tp
      case tp: TypeVar => tp.underlying.underlyingClassRef
      case tp: AnnotatedType => tp.underlying.underlyingClassRef
      case tp: RefinedType => tp.underlying.underlyingClassRef
      case _ => NoType
    }

    /** A prefix-less termRef to a new skolem symbol that has the given type as info */
    def narrow(implicit ctx: Context): TermRef = TermRef(NoPrefix, ctx.newSkolem(this))

 // ----- Normalizing typerefs over refined types ----------------------------

    /** If this is a refinement type that has a refinement for `name` (which might be followed
     *  by other refinements), and the refined info is a type alias, return the alias,
     *  otherwise return NoType. Used to reduce types of the form
     *
     *    P { ... type T = / += / -= U ... } # T
     *
     *  to just U
     */
    def lookupRefined(name: Name)(implicit ctx: Context): Type = stripTypeVar match {
      case pre: RefinedType =>
        if (pre.refinedName ne name) pre.parent.lookupRefined(name)
        else pre.refinedInfo match {
          case TypeBounds(lo, hi) /*if lo eq hi*/ => hi
          case _ => NoType
        }
      case pre: WildcardType =>
        WildcardType
      case _ =>
        NoType
    }

    /** The type <this . name> , reduced if possible */
    def select(name: Name)(implicit ctx: Context): Type = name match {
      case name: TermName =>
        TermRef(this, name)
      case name: TypeName =>
        val res = lookupRefined(name)
        if (res.exists) res else TypeRef(this, name)
    }

    /** The type <this . name> , reduced if possible, with given denotation if unreduced */
    def select(name: Name, denot: Denotation)(implicit ctx: Context): Type = name match {
      case name: TermName =>
        TermRef(this, name, denot)
      case name: TypeName =>
        val res = lookupRefined(name)
        if (res.exists) res else TypeRef(this, name, denot)
    }

    /** The type <this . name> , reduced if possible, with given denotation if unreduced */
    def selectNonMember(name: Name, denot: Denotation)(implicit ctx: Context): Type = name match {
      case name: TermName =>
        TermRef(this, name, denot)
      case name: TypeName =>
        val res = lookupRefined(name)
        if (res.exists) res else TypeRef(this, name, denot)
    }

    /** The type <this . name> with given symbol, reduced if possible */
    def select(sym: Symbol)(implicit ctx: Context): Type =
      if (sym.isTerm) TermRef(this, sym.asTerm)
      else {
        val res = lookupRefined(sym.name)
        if (res.exists) res else TypeRef(this, sym.asType)
      }

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

    /** The first parent of this type, AnyRef if list of parents is empty */
    def firstParent(implicit ctx: Context): TypeRef = parents match {
      case p :: _ => p
      case _ => defn.AnyClass.typeRef
    }

    /** The parameter types of a PolyType or MethodType, Empty list for others */
    final def paramTypess: List[List[Type]] = this match {
      case mt: MethodType => mt.paramTypes :: mt.resultType.paramTypess
      case pt: PolyType => pt.resultType.paramTypess
      case _ => Nil
    }

    /** The parameter types in the first parameter section of a PolyType or MethodType, Empty list for others */
    final def firstParamTypes: List[Type] = this match {
      case mt: MethodType => mt.paramTypes
      case pt: PolyType => pt.resultType.firstParamTypes
      case _ => Nil
    }

    /** Is this either not a method at all, or a parameterless method? */
    final def isParameterless: Boolean = this match {
      case mt: MethodType => false
      case pt: PolyType => pt.resultType.isParameterless
      case _ => true
    }

    /** The resultType of a PolyType, MethodType, or ExprType, the type itself for others */
    def resultType: Type = this

    /** The final result type of a PolyType, MethodType, or ExprType, after skipping
     *  all parameter sections, the type itself for all others.
     */
    def finalResultType: Type = resultType match {
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
      classSymbol.decls.lookup(name) orElse member(name).symbol

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
          if (from2.isEmpty) ctx.subst2(this, from.head, to.head, from.tail.head, to.tail.head, null)
          else ctx.subst(this, from, to, null)
        }
      }

    /** Substitute all types of the form `PolyParam(from, N)` by
     *  `PolyParam(to, N)`.
     */
    final def subst(from: BindingType, to: BindingType)(implicit ctx: Context): Type =
      ctx.subst(this, from, to, null)

    /** Substitute all occurrences of `This(cls)` by `tp` */
    final def substThis(cls: ClassSymbol, tp: Type)(implicit ctx: Context): Type =
      ctx.substThis(this, cls, tp, null)

    /** Substitute all occurrences of `RefinedThis(rt)` by `tp` */
    final def substThis(rt: RefinedType, tp: Type)(implicit ctx: Context): Type =
      ctx.substThis(this, rt, tp, null)

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
     */
    def toFunctionType(implicit ctx: Context): Type = this match {
      case mt @ MethodType(_, formals) if !mt.isDependent =>
        defn.FunctionType(formals mapConserve (_.underlyingIfRepeated), mt.resultType)
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

    type VarianceMap = SimpleMap[TypeVar, Integer]

    /** All occurrences of type vars in this type that satisfy predicate
     *  `include` mapped to their variances (-1/0/1) in this type, where
     *  -1 means: only covariant occurrences
     *  +1 means: only covariant occurrences
     *  0 means: mixed or non-variant occurrences
     */
    def variances(include: TypeVar => Boolean)(implicit ctx: Context): VarianceMap = track("variances") {
      val accu = new TypeAccumulator[VarianceMap] {
        def apply(vmap: VarianceMap, t: Type): VarianceMap = t match {
          case t: TypeVar
          if !t.isInstantiated && (ctx.typerState.constraint contains t) && include(t) =>
            val v = vmap(t)
            if (v == null) vmap.updated(t, variance)
            else if (v == variance) vmap
            else vmap.updated(t, 0)
          case _ =>
            foldOver(vmap, t)
        }
      }
      accu(SimpleMap.Empty, this)
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

  /** Implementations of this trait cache the resukts of `narrow`. */
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

    def knownDenotation: Boolean = lastDenotation != null

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
      val d = lastDenotation match {
        case null =>
          val sym = lastSymbol
          if (sym == null) loadDenot else denotOfSym(sym)
        case d: SymDenotation =>
          if (d.validFor.runId == ctx.runId || ctx.stillValid(d)) d.current
          else {
            val newd = loadDenot
            if (newd.exists) newd else d.staleSymbolError
          }
        case d =>
          if (d.validFor.runId == ctx.period.runId) d.current
          else loadDenot
      }
      lastDenotation = d
      lastSymbol = d.symbol
      checkedPeriod = ctx.period
      d
    }

    private def denotOfSym(sym: Symbol)(implicit ctx: Context): Denotation = {
      val d = sym.denot
      val owner = d.owner
      if (owner.isTerm) d else d.asSeenFrom(prefix)
    }

    private def checkSymAssign(sym: Symbol) =
      assert(
        (lastSymbol eq sym) ||
        (lastSymbol eq null) ||
        (lastSymbol.defRunId != sym.defRunId) ||
        (lastSymbol.defRunId == NoRunId),
        s"data race? overwriting symbol of $this / ${this.getClass} / ${lastSymbol.id} / ${sym.id}")

    protected def sig: Signature = Signature.NotAMethod

    private[dotc] def withDenot(denot: Denotation)(implicit ctx: Context): ThisType =
      if (sig != denot.signature)
        withSig(denot.signature).withDenot(denot).asInstanceOf[ThisType]
      else {
        setDenot(denot)
        this
      }

    private[dotc] final def setDenot(denot: Denotation)(implicit ctx: Context): Unit = {
      if (Config.checkTermRefs)
        if (ctx.settings.YnoDoubleBindings.value)
          checkSymAssign(denot.symbol)
      lastDenotation = denot
      lastSymbol = denot.symbol
    }

    private[dotc] def withSym(sym: Symbol, signature: Signature)(implicit ctx: Context): ThisType =
      if (sig != signature)
        withSig(signature).withSym(sym, signature).asInstanceOf[ThisType]
      else {
        setSym(sym)
        this
      }

    private[dotc] final def setSym(sym: Symbol)(implicit ctx: Context): Unit = {
      if (Config.checkTermRefs)
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

    protected def loadDenot(implicit ctx: Context) = {
      val d = prefix.member(name)
      if (d.exists || ctx.phaseId == FirstPhaseId)
        d
      else {// name has changed; try load in earlier phase and make current
        val d = denot(ctx.withPhase(ctx.phaseId - 1)).current
        if (d.exists) d
        else throw new Error(s"failure to reload $this")
      }
    }

    def symbol(implicit ctx: Context): Symbol = {
      val now = ctx.period
      if (checkedPeriod == now ||
          lastDenotation == null && lastSymbol != null) lastSymbol
      else denot.symbol
    }

    def info(implicit ctx: Context): Type = denot.info

    def isType = isInstanceOf[TypeRef]
    def isTerm = isInstanceOf[TermRef]

    /** Guard against cycles that can arise if given `op`
     *  follows info. The prblematic cases are a type alias to itself or
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
      if (ctx.underlyingRecursions < LogPendingUnderlyingThreshold)
        op
      else if (ctx.pendingUnderlying contains this)
        throw new CyclicReference(symbol)
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

    def derivedSelect(prefix: Type)(implicit ctx: Context): Type =
      if (prefix eq this.prefix) this
      else {
        val res = prefix.lookupRefined(name)
        if (res.exists) res else newLikeThis(prefix)
      }

    /** Create a NamedType of the same kind as this type, but with a new prefix.
     */
    protected def newLikeThis(prefix: Type)(implicit ctx: Context): NamedType =
      NamedType(prefix, name)

    override def equals(that: Any) = that match {
      case that: NamedType =>
        this.name == that.name &&
        this.prefix == that.prefix &&
        !that.isInstanceOf[TermRefWithSignature]
      case _ =>
        false
    }
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
      TermRef.withSig(prefix, name, sd.signature, sd)

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
      else d.atSignature(sig)
    }

    override def newLikeThis(prefix: Type)(implicit ctx: Context): TermRef = {
      if (sig != Signature.NotAMethod &&
          sig != Signature.OverloadedSignature &&
          symbol.exists) {
        val ownSym = symbol
        TermRef(prefix, name).withDenot(prefix.member(name).disambiguate(_ eq ownSym))
      }
      else TermRef.withSig(prefix, name, sig)
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

  trait WithNonMemberSym extends NamedType {
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

    override def equals(that: Any) = that match {
      case that: WithNonMemberSym => this.prefix == that.prefix && (this.fixedSym eq that.fixedSym)
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

  final class NonMemberTermRef(prefix: Type, name: TermName, val fixedSym: TermSymbol) extends TermRef(prefix, name) with WithNonMemberSym
  final class NonMemberTypeRef(prefix: Type, name: TypeName, val fixedSym: TypeSymbol) extends TypeRef(prefix, name) with WithNonMemberSym

  object NamedType {
    def apply(prefix: Type, name: Name)(implicit ctx: Context) =
      if (name.isTermName) TermRef(prefix, name.asTermName)
      else TypeRef(prefix, name.asTypeName)
    def apply(prefix: Type, name: Name, denot: Denotation)(implicit ctx: Context) =
      if (name.isTermName) TermRef(prefix, name.asTermName, denot)
      else TypeRef(prefix, name.asTypeName, denot)
    def withNonMemberSym(prefix: Type, sym: Symbol)(implicit ctx: Context) =
      if (sym.isType) TypeRef.withNonMemberSym(prefix, sym.name.asTypeName, sym.asType)
      else TermRef.withNonMemberSym(prefix, sym.name.asTermName, sym.asTerm)
  }

  object TermRef {
    def apply(prefix: Type, name: TermName)(implicit ctx: Context): TermRef =
      ctx.uniqueNamedTypes.enterIfNew(prefix, name).asInstanceOf[TermRef]

    def apply(prefix: Type, sym: TermSymbol)(implicit ctx: Context): TermRef =
      withSymAndName(prefix, sym, sym.name)

    def apply(prefix: Type, name: TermName, denot: Denotation)(implicit ctx: Context): TermRef = {
      if (prefix eq NoPrefix) apply(prefix, denot.symbol.asTerm)
      else denot match {
        case denot: SymDenotation if denot.isCompleted => withSig(prefix, name, denot.signature)
        case _ => apply(prefix, name)
      }
    } withDenot denot

    def withNonMemberSym(prefix: Type, name: TermName, sym: TermSymbol)(implicit ctx: Context): TermRef =
      unique(new NonMemberTermRef(prefix, name, sym))

    def withSymAndName(prefix: Type, sym: TermSymbol, name: TermName)(implicit ctx: Context): TermRef =
      if (prefix eq NoPrefix) withNonMemberSym(prefix, name, sym)
      else {
        if (sym.defRunId != NoRunId && sym.isCompleted) withSig(prefix, name, sym.signature)
        else  apply(prefix, name)
      } withSym (sym, Signature.NotAMethod)

    def withSig(prefix: Type, sym: TermSymbol)(implicit ctx: Context): TermRef =
      unique(withSig(prefix, sym.name, sym.signature).withSym(sym, sym.signature))

    def withSig(prefix: Type, name: TermName, sig: Signature)(implicit ctx: Context): TermRef =
      unique(new TermRefWithSignature(prefix, name, sig))

    def withSig(prefix: Type, name: TermName, sig: Signature, denot: Denotation)(implicit ctx: Context): TermRef =
      (if (prefix eq NoPrefix) apply(prefix, denot.symbol.asTerm)
       else withSig(prefix, name, sig)) withDenot denot
  }

  object TypeRef {
    def apply(prefix: Type, name: TypeName)(implicit ctx: Context): TypeRef =
      ctx.uniqueNamedTypes.enterIfNew(prefix, name).asInstanceOf[TypeRef]

    def apply(prefix: Type, sym: TypeSymbol)(implicit ctx: Context): TypeRef =
      withSymAndName(prefix, sym, sym.name)

    def withNonMemberSym(prefix: Type, name: TypeName, sym: TypeSymbol)(implicit ctx: Context): TypeRef =
      unique(new NonMemberTypeRef(prefix, name, sym))

    def withSymAndName(prefix: Type, sym: TypeSymbol, name: TypeName)(implicit ctx: Context): TypeRef =
      if (prefix eq NoPrefix) withNonMemberSym(prefix, name, sym)
      else apply(prefix, name).withSym(sym, Signature.NotAMethod)

    def apply(prefix: Type, name: TypeName, denot: Denotation)(implicit ctx: Context): TypeRef =
      (if (prefix eq NoPrefix) apply(prefix, denot.symbol.asType) else apply(prefix, name)) withDenot denot
  }

  // --- Other SingletonTypes: ThisType/SuperType/ConstantType ---------------------------

  /** The type cls.this */
  abstract case class ThisType(cls: ClassSymbol) extends CachedProxyType with SingletonType {
    override def underlying(implicit ctx: Context) = cls.classInfo.selfType
    override def computeHash = doHash(cls)
  }

  final class CachedThisType(cls: ClassSymbol) extends ThisType(cls)

  // TODO: consider hash before constructing types?
  object ThisType {
    def apply(cls: ClassSymbol)(implicit ctx: Context) =
      unique(new CachedThisType(cls))
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
    def apply(thistpe: Type, supertpe: Type)(implicit ctx: Context) =
      unique(new CachedSuperType(thistpe, supertpe))
  }

  /** A constant type with  single `value`. */
  abstract case class ConstantType(value: Constant) extends CachedProxyType with SingletonType {
    override def underlying(implicit ctx: Context) = value.tpe
    override def computeHash = doHash(value)
  }

  final class CachedConstantType(value: Constant) extends ConstantType(value)

  object ConstantType {
    def apply(value: Constant)(implicit ctx: Context) =
      unique(new CachedConstantType(value))
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

    override def underlying(implicit ctx: Context) = parent

    /** Derived refined type, with a twist: A refinement with a higher-kinded type param placeholder
     *  is transformed to a refinement of the original type parameter if that one exists.
     */
    def derivedRefinedType(parent: Type, refinedName: Name, refinedInfo: Type)(implicit ctx: Context): RefinedType = {
      lazy val underlyingTypeParams = parent.safeUnderlyingTypeParams
      lazy val originalTypeParam = underlyingTypeParams(refinedName.hkParamIndex)

      /** Use variance of newly instantiated type parameter rather than the old hk argument
       */
      def adjustedHKRefinedInfo(hkBounds: TypeBounds, underlyingTypeParam: TypeSymbol) = hkBounds match {
        case tp @ TypeBounds(lo, hi) if lo eq hi =>
          tp.derivedTypeBounds(lo, hi, underlyingTypeParam.variance)
        case _ =>
          hkBounds
      }

      if ((parent eq this.parent) && (refinedName eq this.refinedName) && (refinedInfo eq this.refinedInfo))
        this
      else if (   refinedName.isHkParamName
 //            && { println(s"deriving $refinedName $parent $underlyingTypeParams"); true }
               && refinedName.hkParamIndex < underlyingTypeParams.length
               && originalTypeParam.name != refinedName)
        derivedRefinedType(parent, originalTypeParam.name,
            adjustedHKRefinedInfo(refinedInfo.bounds, underlyingTypeParams(refinedName.hkParamIndex)))
      else
        RefinedType(parent, refinedName, rt => refinedInfo.substThis(this, RefinedThis(rt)))
    }

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

    def apply(parent: Type, name: Name, infoFn: RefinedType => Type)(implicit ctx: Context): RefinedType =
      ctx.base.uniqueRefinedTypes.enterIfNew(new CachedRefinedType(parent, name, infoFn))

    def apply(parent: Type, name: Name, info: Type)(implicit ctx: Context): RefinedType = {
      ctx.base.uniqueRefinedTypes.enterIfNew(parent, name, info)
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
      unique(new CachedAndType(tp1, tp2))
    }
    def make(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if (tp1 eq tp2) tp1 else apply(tp1, tp2)
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
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context) =
      unique(new CachedOrType(tp1, tp2))
    def make(tp1: Type, tp2: Type)(implicit ctx: Context): Type =
      if (tp1 eq tp2) tp1 else apply(tp1, tp2)
  }

  // ----- Method types: MethodType/ExprType/PolyType -------------------------------

  // Note: method types are cached whereas poly types are not. The reason
  // is that most poly types are cyclic via poly params,
  // and therefore two different poly types would never be equal.

  /** A trait that mixes in functionality for signature caching */
  trait SignedType extends Type {

    private[this] var mySignature: Signature = _
    private[this] var mySignatureRunId: Int = NoRunId

    protected def computeSignature(implicit ctx: Context): Signature

    protected def resultSignature(implicit ctx: Context) = resultType match {
      case rtp: SignedType => rtp.signature
      case tp => Signature(tp, isJava = false)
    }

    final override def signature(implicit ctx: Context): Signature = {
      if (ctx.runId != mySignatureRunId) {
        mySignature = computeSignature
        mySignatureRunId = ctx.runId
      }
      mySignature
    }
  }

  trait MethodOrPoly extends SignedType

  abstract case class MethodType(paramNames: List[TermName], paramTypes: List[Type])
      (resultTypeExp: MethodType => Type)
    extends CachedGroundType with BindingType with TermType with MethodOrPoly with NarrowCached { thisMethodType =>

    override val resultType = resultTypeExp(this)
    assert(resultType != NoType)
    def isJava = false
    def isImplicit = false

    private[this] var myIsDependent: Boolean = _
    private[this] var myIsDepKnown = false

    /** Does result type contain references to parameters of this method type?
     */
    def isDependent(implicit ctx: Context) = {
      if (!myIsDepKnown) {
        val isDepAcc = new TypeAccumulator[Boolean] {
          def apply(x: Boolean, tp: Type) = x || {
            tp match {
              case MethodParam(`thisMethodType`, _) => true
              case tp @ TypeRef(MethodParam(`thisMethodType`, _), name) =>
                tp.info match { // follow type arguments to avoid dependency
                  case TypeBounds(lo, hi) if lo eq hi => apply(x, hi)
                  case _ => true
                }
              case _ =>
                foldOver(x, tp)
            }
          }
        }
        myIsDependent = isDepAcc(false, resultType)
        myIsDepKnown = true
      }
      myIsDependent
    }

    protected def computeSignature(implicit ctx: Context): Signature =
      resultSignature.prepend(paramTypes, isJava)

    def derivedMethodType(paramNames: List[TermName], paramTypes: List[Type], restpe: Type)(implicit ctx: Context) =
      if ((paramNames eq this.paramNames) && (paramTypes eq this.paramTypes) && (restpe eq this.resultType)) this
      else {
        val restpeFn = (x: MethodType) => restpe.subst(this, x)
        if (isJava) JavaMethodType(paramNames, paramTypes)(restpeFn)
        else if (isImplicit) ImplicitMethodType(paramNames, paramTypes)(restpeFn)
        else MethodType(paramNames, paramTypes)(restpeFn)
      }

    def instantiate(argTypes: => List[Type])(implicit ctx: Context): Type =
      if (isDependent) resultType.substParams(this, argTypes)
      else resultType

    override def equals(that: Any) = that match {
      case that: MethodType =>
        this.paramNames == that.paramNames &&
        this.paramTypes == that.paramTypes &&
        this.resultType == that.resultType
      case _ =>
        false
    }

    override def computeHash = doHash(paramNames, resultType, paramTypes)

    protected def prefixString = "MethodType"
    override def toString = s"$prefixString($paramNames, $paramTypes, $resultType)"
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
    def apply(paramTypes: List[Type], resultType: Type)(implicit ctx: Context): MethodType =
      apply(nme.syntheticParamNames(paramTypes.length), paramTypes, resultType)
    def fromSymbols(params: List[Symbol], resultType: Type)(implicit ctx: Context) = {
      def transformResult(mt: MethodType) =
        resultType.subst(params, (0 until params.length).toList map (MethodParam(mt, _)))
      apply(params map (_.name.asTermName), params map (_.info))(transformResult _)
    }
  }

  object MethodType extends MethodTypeCompanion {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context) =
      unique(new CachedMethodType(paramNames, paramTypes)(resultTypeExp))
  }

  object JavaMethodType extends MethodTypeCompanion {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context) =
      unique(new JavaMethodType(paramNames, paramTypes)(resultTypeExp))
  }

  object ImplicitMethodType extends MethodTypeCompanion {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context) =
      unique(new ImplicitMethodType(paramNames, paramTypes)(resultTypeExp))
  }

  abstract case class ExprType(override val resultType: Type)
  extends CachedProxyType with TermType with SignedType {
    override def underlying(implicit ctx: Context): Type = resultType
    protected def computeSignature(implicit ctx: Context): Signature = resultSignature
    def derivedExprType(resultType: Type)(implicit ctx: Context) =
      if (resultType eq this.resultType) this else ExprType(resultType)
    override def computeHash = doHash(resultType)
  }

  final class CachedExprType(resultType: Type) extends ExprType(resultType)

  object ExprType {
    def apply(resultType: Type)(implicit ctx: Context) =
      unique(new CachedExprType(resultType))
  }

  case class PolyType(paramNames: List[TypeName])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type)
    extends CachedGroundType with BindingType with TermType with MethodOrPoly {

    val paramBounds = paramBoundsExp(this)
    override val resultType = resultTypeExp(this)

    protected def computeSignature(implicit ctx: Context) = resultSignature

    def instantiate(argTypes: List[Type])(implicit ctx: Context): Type =
      resultType.substParams(this, argTypes)

    def instantiateBounds(argTypes: List[Type])(implicit ctx: Context): List[TypeBounds] =
      paramBounds.mapConserve(_.substParams(this, argTypes).bounds)

    def derivedPolyType(paramNames: List[TypeName], paramBounds: List[TypeBounds], restpe: Type)(implicit ctx: Context) =
      if ((paramNames eq this.paramNames) && (paramBounds eq this.paramBounds) && (restpe eq this.resultType)) this
      else copy(paramNames, paramBounds, restpe)

    def copy(paramNames: List[TypeName], paramBounds: List[TypeBounds], restpe: Type)(implicit ctx: Context) =
      PolyType(paramNames)(
          x => paramBounds mapConserve (_.subst(this, x).bounds),
          x => restpe.subst(this, x))

    // need to override hashCode and equals to be object identity
    // because paramNames by itself is not discriminatory enough
    override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]
    override def computeHash = identityHash

    override def toString = s"PolyType($paramNames, $paramBounds, $resultType)"
  }

  object PolyType {
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

  // ----- Bound types: MethodParam, PolyParam, RefiendThis --------------------------

  abstract class BoundType extends CachedProxyType with ValueType {
    type BT <: BindingType
    def binder: BT
    // Dotty deviation: copyBoundType was copy, but
    // dotty generates copy methods always automatically, and therefore
    // does not accept same-named method definitions in subclasses.
    // Scala2x, on the other hand, requires them (not sure why!)
    def copyBoundType(bt: BT): Type
  }

  abstract class ParamType extends BoundType {
    def paramNum: Int
  }

  case class MethodParam(binder: MethodType, paramNum: Int) extends ParamType with SingletonType {
    type BT = MethodType
    override def underlying(implicit ctx: Context): Type = binder.paramTypes(paramNum)
    def copyBoundType(bt: BT) = MethodParam(bt, paramNum)

    // need to customize hashCode and equals to prevent infinite recursion for dep meth types.
    override def computeHash = addDelta(System.identityHashCode(binder), paramNum)
    override def equals(that: Any) = that match {
      case that: MethodParam =>
        (this.binder eq that.binder) && this.paramNum == that.paramNum
      case _ =>
        false
    }

    override def toString = s"MethodParam(${binder.paramNames(paramNum)})"
  }

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

    override def underlying(implicit ctx: Context): Type = binder.paramBounds(paramNum)
    // no customized hashCode/equals needed because cycle is broken in PolyType
    override def toString = s"PolyParam(${binder.paramNames(paramNum)})"

    override def computeHash = doHash(paramNum, binder)
  }

  case class RefinedThis(binder: RefinedType) extends BoundType with SingletonType {
    type BT = RefinedType
    override def underlying(implicit ctx: Context) = binder.parent
    def copyBoundType(bt: BT) = RefinedThis(bt)

    // need to customize hashCode and equals to prevent infinite recursion for
    // refinements that refer to the refinement type via this
    override def computeHash = identityHash
    override def equals(that: Any) = that match {
      case that: RefinedThis => this.binder eq that.binder
      case _ => false
    }
    override def toString = s"RefinedThis(${binder.hashCode})"
  }

  // ------------ Type variables ----------------------------------------

  /** A type variable is essentially a switch that models some part of a substitution.
   *  It is first linked to `origin`, a poly param that's in the current constraint set.
   *  It can then be (once) instantiated to some other type. The instantiation is
   *  recorded in the type variable itself, or else, if the current type state
   *  is different from the variable's creation state (meaning unrolls are possible)
   *  in the current typer state. Every type variable is referred to by exactly
   *  one inferred type parameter in a TypeApply tree.
   *
   *  @param  origin        The parameter that's tracked by the type variable.
   *  @param  creatorState  The typer state in which the variable was created.
   *  @param  owningTree    The function part of the TypeApply tree tree that introduces
   *                        the type variable.
   */
  final class TypeVar(val origin: PolyParam, creatorState: TyperState, val owningTree: untpd.Tree) extends CachedProxyType with ValueType {

    /** The permanent instance type of the the variable, or NoType is none is given yet */
    private[core] var inst: Type = NoType

    /** The state owning the variable. This is at first `creatorState`, but it can
     *  be changed to an enclosing state on a commit.
     */
    private[core] var owningState = creatorState

    /** The instance type of this variable, or NoType if the variable is currently
     *  uninstantiated
     */
    def instanceOpt(implicit ctx: Context): Type =
      if (inst.exists) inst else ctx.typerState.instType(this)

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
      def upperBound = ctx.typerState.constraint.bounds(origin).hi
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

      // Then, approximate by (1.) and (2.) and simplify as follows.
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
      if (inst.exists) inst else origin
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

    def selfType(implicit ctx: Context): Type = selfInfo match {
      case NoType =>
        if (selfTypeCache == null) selfTypeCache = computeSelfType(cls.typeRef, cls.typeParams)
        selfTypeCache
      case tp: Type => tp
      case self: Symbol => self.info
    }

    private var selfTypeCache: Type = null

    private def computeSelfType(base: Type, tparams: List[TypeSymbol])(implicit ctx: Context): Type = tparams match {
      case tparam :: tparams1 =>
        computeSelfType(
          RefinedType(base, tparam.name, TypeRef(cls.thisType, tparam).toBounds(tparam)),
          tparams1)
      case nil =>
        base
    }

    def rebase(tp: Type)(implicit ctx: Context): Type =
      if ((prefix eq cls.owner.thisType) || !cls.owner.isClass) tp
      else tp.substThis(cls.owner.asClass, prefix)

    private var typeRefCache: Type = null

    def typeRef(implicit ctx: Context): Type = {
      def clsDenot = if (prefix eq cls.owner.thisType) cls.denot else cls.denot.copySymDenotation(info = this)
      if (typeRefCache == null)
        typeRefCache =
          if ((cls is PackageClass) || cls.owner.isTerm) prefix select cls
          else prefix select (cls.name, clsDenot)
      typeRefCache
    }

    // cached because baseType needs parents
    private var parentsCache: List[TypeRef] = null

    override def parents(implicit ctx: Context): List[TypeRef] = {
      if (parentsCache == null)
        parentsCache = cls.classParents.mapConserve(rebase(_).asInstanceOf[TypeRef])
      parentsCache
    }

    def derivedClassInfo(prefix: Type)(implicit ctx: Context) =
      if (prefix eq this.prefix) this
      else ClassInfo(prefix, cls, classParents, decls, selfInfo)

    def derivedClassInfo(prefix: Type = this.prefix, classParents: List[TypeRef] = classParents, selfInfo: DotClass = this.selfInfo)(implicit ctx: Context) =
      if ((prefix eq this.prefix) && (classParents eq this.classParents) && (selfInfo eq this.selfInfo)) this
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

  /** Type bounds >: lo <: hi */
  abstract case class TypeBounds(lo: Type, hi: Type) extends CachedProxyType with TypeType {

    assert(lo.isInstanceOf[TermType])
    assert(hi.isInstanceOf[TermType])

    def variance: Int = 0

    override def underlying(implicit ctx: Context): Type = hi

    def derivedTypeBounds(lo: Type, hi: Type, variance: Int = this.variance)(implicit ctx: Context) =
      if ((lo eq this.lo) && (hi eq this.hi) && (variance == this.variance)) this
      else TypeBounds(lo, hi, variance)

    /** pre: this is a type alias */
    def derivedTypeAlias(tp: Type, variance: Int = this.variance)(implicit ctx: Context) =
      if (lo eq tp) this
      else TypeAlias(tp, variance)

    def contains(tp: Type)(implicit ctx: Context) = tp match {
      case tp: TypeBounds => lo <:< tp.lo && tp.hi <:< hi
      case _ => lo <:< tp && tp <:< hi
    }

    def & (that: TypeBounds)(implicit ctx: Context): TypeBounds = {
      val v = this commonVariance that
      if (v != 0 && (this.lo eq this.hi) && (that.lo eq that.hi))
        if (v > 0) derivedTypeAlias(this.hi & that.hi, v)
        else derivedTypeAlias(this.lo | that.lo, v)
      else derivedTypeBounds(this.lo | that.lo, this.hi & that.hi, v)
    }

    def | (that: TypeBounds)(implicit ctx: Context): TypeBounds = {
      val v = this commonVariance that
      if (v != 0 && (this.lo eq this.hi) && (that.lo eq that.hi))
        if (v > 0) derivedTypeAlias(this.hi | that.hi, v)
        else derivedTypeAlias(this.lo & that.lo, v)
      else derivedTypeBounds(this.lo & that.lo, this.hi | that.hi, v)
    }

    override def & (that: Type)(implicit ctx: Context) = that match {
      case that: TypeBounds => this & that
      case _ => super.& (that)
    }

    override def | (that: Type)(implicit ctx: Context) = that match {
      case that: TypeBounds => this | that
      case _ => super.| (that)
    }

    /** If this type and that type have the same variance, this variance, otherwise 0 */
    final def commonVariance(that: TypeBounds): Int = (this.variance + that.variance) / 2

    /** Given a the typebounds L..H of higher-kinded abstract type
     *
     *    type T[boundSyms] >: L <: H
     *
     *  produce its equivalent bounds L'..R that make no reference to the bound
     *  symbols on the left hand side. The idea is to rewrite the declaration to
     *
     *      type T >: L' <: HigherKindedXYZ { type _$hk$i >: bL_i <: bH_i } & H'
     *
     *  where
     *
     *  - XYZ encodes the variants of the bound symbols using `P` (positive variance)
     *    `N` (negative variance), `I` (invariant).
     *  - bL_i is the lower bound of bound symbol #i under substitution `substBoundSyms`
     *  - bH_i is the upper bound of bound symbol #i under substitution `substBoundSyms`
     *  - `substBoundSyms` is the substitution that maps every bound symbol #i to the
     *    reference `<this>._$hk$i`, where `<this>` is the RefinedThis referring to the
     *    previous HigherKindedXYZ refined type.
     *  - L' = substBoundSyms(L), H' = substBoundSyms(H)
     *
     *  Example:
     *
     *      type T[X <: F[X]] <: Traversable[X, T]
     *
     *  is rewritten to:
     *
     *      type T <: HigherKindedP { type _$hk$0 <: F[$_hk$0] } & Traversable[<this>._$hk$0, T]
     *
     *  @see Definitions.hkTrait
     */
    def higherKinded(boundSyms: List[Symbol])(implicit ctx: Context): TypeBounds = {
      val parent = defn.hkTrait(boundSyms map (_.variance)).typeRef
      val hkParamNames = boundSyms.indices.toList map tpnme.higherKindedParamName
      def substBoundSyms(tp: Type)(rt: RefinedType): Type =
        tp.subst(boundSyms, hkParamNames map (TypeRef(RefinedThis(rt), _)))
      val hkParamInfoFns: List[RefinedType => Type] =
        for (bsym <- boundSyms) yield substBoundSyms(bsym.info) _
      val hkBound = RefinedType.make(parent, hkParamNames, hkParamInfoFns).asInstanceOf[RefinedType]
      TypeBounds(substBoundSyms(lo)(hkBound), AndType(hkBound, substBoundSyms(hi)(hkBound)))
    }

    override def toString =
      if (lo eq hi) s"TypeAlias($lo)" else s"TypeBounds($lo, $hi)"
  }

  class CachedTypeBounds(lo: Type, hi: Type, hc: Int) extends TypeBounds(lo, hi) {
    myHash = hc
    override def computeHash = unsupported("computeHash")
  }

  final class CoTypeBounds(lo: Type, hi: Type, hc: Int) extends CachedTypeBounds(lo, hi, hc) {
    override def variance = 1
    override def toString = "Co" + super.toString
  }
  final class ContraTypeBounds(lo: Type, hi: Type, hc: Int) extends CachedTypeBounds(lo, hi, hc) {
    override def variance = -1
    override def toString = "Contra" + super.toString
  }

  object TypeBounds {
    def apply(lo: Type, hi: Type, variance: Int = 0)(implicit ctx: Context): TypeBounds =
      ctx.uniqueTypeBounds.enterIfNew(lo, hi, variance)
    def empty(implicit ctx: Context) = apply(defn.NothingType, defn.AnyType)
    def upper(hi: Type, variance: Int = 0)(implicit ctx: Context) = apply(defn.NothingType, hi, variance)
    def lower(lo: Type, variance: Int = 0)(implicit ctx: Context) = apply(lo, defn.AnyType, variance)
  }

  object TypeAlias {
    def apply(tp: Type, variance: Int = 0)(implicit ctx: Context) = TypeBounds(tp, tp, variance)
    def unapply(tp: Type): Option[Type] = tp match {
      case TypeBounds(lo, hi) if lo eq hi => Some(lo)
      case _ => None
    }
  }

  // ----- Annotated and Import types -----------------------------------------------

  /** An annotated type tpe @ annot */
  case class AnnotatedType(annot: Annotation, tpe: Type)
      extends UncachedProxyType with ValueType {
    // todo: cache them? but this makes only sense if annotations and trees are also cached.
    override def underlying(implicit ctx: Context): Type = tpe
    def derivedAnnotatedType(annot: Annotation, tpe: Type) =
      if ((annot eq this.annot) && (tpe eq this.tpe)) this
      else AnnotatedType(annot, tpe)
  }

  object AnnotatedType {
    def make(annots: List[Annotation], underlying: Type) =
      if (annots.isEmpty) underlying
      else (underlying /: annots)((tp, ann) => AnnotatedType(ann, tp))
  }

  // Special type objects and classes -----------------------------------------------------

  /** The type of an import clause tree */
  case class ImportType(expr: Tree) extends UncachedGroundType

  /** Sentinal for "missing type" */
  case object NoType extends CachedGroundType {
    override def exists = false
    override def computeHash = hashSeed
  }

  /** Missing prefix */
  case object NoPrefix extends CachedGroundType {
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

  object WildcardType extends WildcardType(NoType) {
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
            case _=> None
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

    /** Map this function over given type */
    def mapOver(tp: Type): Type = {
      implicit val ctx = this.ctx
      tp match {
        case tp: NamedType =>
          if (stopAtStatic && tp.symbol.isStatic) tp
          else tp.derivedSelect(this(tp.prefix))

        case _: ThisType
          | _: BoundType
          | NoPrefix => tp

        case tp: RefinedType =>
          tp.derivedRefinedType(this(tp.parent), tp.refinedName, this(tp.refinedInfo))

        case tp: TypeBounds =>
          def mapOverBounds = {
            val lo = tp.lo
            val hi = tp.hi
            if (lo eq hi) {
              val saved = variance
              variance = variance * tp.variance
              val lo1 = this(lo)
              variance = saved
              tp.derivedTypeAlias(lo1)
            } else {
              variance = -variance
              val lo1 = this(lo)
              variance = -variance
              tp.derivedTypeBounds(lo1, this(hi))
            }
          }
          mapOverBounds

        case tp: MethodType =>
          def mapOverMethod = {
            variance = -variance
            val ptypes1 = tp.paramTypes mapConserve this
            variance = -variance
            tp.derivedMethodType(tp.paramNames, ptypes1, this(tp.resultType))
          }
          mapOverMethod

        case tp: ExprType =>
          tp.derivedExprType(this(tp.resultType))

        case tp: PolyType =>
          def mapOverPoly = {
            variance = -variance
            val bounds1 = tp.paramBounds.mapConserve(this).asInstanceOf[List[TypeBounds]]
            variance = -variance
            tp.derivedPolyType(
              tp.paramNames, bounds1, this(tp.resultType))
          }
          mapOverPoly

        case tp @ SuperType(thistp, supertp) =>
          tp.derivedSuperType(this(thistp), this(supertp))

        case tp: ClassInfo =>
          mapClassInfo(tp)

        case tp: TypeVar =>
          val inst = tp.instanceOpt
          if (inst.exists) apply(inst) else tp

        case tp: AndOrType =>
          tp.derivedAndOrType(this(tp.tp1), this(tp.tp2))

        case tp @ AnnotatedType(annot, underlying) =>
          val underlying1 = this(underlying)
          if (underlying1 eq underlying) tp else underlying1

        case tp @ WildcardType =>
          tp.derivedWildcardType(mapOver(tp.optBounds))

        case tp: ProtoType =>
          tp.map(this)

        case _ =>
          tp
      }
    }

    def mapOver(syms: List[Symbol]): List[Symbol] =
      ctx.mapSymbols(syms, this)

    def mapOver(scope: Scope): Scope = {
      val elems = scope.toList
      val elems1 = mapOver(elems)
      if (elems1 eq elems) scope
      else newScopeWith(elems1: _*)
    }

    def mapOver(annot: Annotation): Annotation =
      annot.derivedAnnotation(mapOver(annot.tree))

    def mapOver(tree: Tree): Tree =
      new TreeTypeMap(this).apply(tree)

    /** Can be overridden. By default, only the prefix is mapped. */
    protected def mapClassInfo(tp: ClassInfo): ClassInfo =
      tp.derivedClassInfo(this(tp.prefix))

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
      val self1 = tp.self match {
        case self: Type => this(self)
        case _ => tp.self
      }
      tp.derivedClassInfo(prefix1, parents1, self1)
    }
  }

  object IdentityTypeMap extends TypeMap()(NoContext) {
    override def stopAtStatic = true
    def apply(tp: Type) = tp
  }

  // ----- TypeAccumulators ----------------------------------------------------

  abstract class TypeAccumulator[T](implicit protected val ctx: Context) extends ((T, Type) => T) {

    protected def stopAtStatic = true

    def apply(x: T, tp: Type): T

    protected def applyToAnnot(x: T, annot: Annotation): T = x // don't go into annotations

    protected var variance = 1

    def foldOver(x: T, tp: Type): T = tp match {
      case tp: TypeRef =>
        if (stopAtStatic && tp.symbol.isStatic) x
        else {
          val tp1 = tp.prefix.lookupRefined(tp.name)
          this(x, if (tp1.exists) tp1 else tp.prefix)
        }
      case tp: TermRef =>
        if (stopAtStatic && tp.symbol.isStatic) x
        else this(x, tp.prefix)

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

      case AnnotatedType(annot, underlying) =>
        this(applyToAnnot(x, annot), underlying)

      case tp: TypeVar =>
        this(x, tp.underlying)

      case tp: WildcardType =>
        this(x, tp.optBounds)

      case tp: ProtoType =>
        tp.fold(x, this)

      case _ => x
    }

    final def foldOver(x: T, ts: List[Type]): T = ts match {
      case t :: ts1 => foldOver(apply(x, t), ts1)
      case nil => x
    }
  }

  class ExistsAccumulator(p: Type => Boolean)(implicit ctx: Context) extends TypeAccumulator[Boolean] {
    override def stopAtStatic = false
    def apply(x: Boolean, tp: Type) = x || p(tp) || foldOver(x, tp)
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
          case tp: ThisType =>
            apply(x, tp.underlying)
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
        (mbr.symbol is Deferred) && {
          mbr.info match {
            case TypeBounds(lo, hi) => lo ne hi
            case _ => false
          }
        }
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
  class FatalTypeError(msg: String) extends TypeError(msg)

  class MalformedType(pre: Type, denot: Denotation, absMembers: Set[Name])
    extends FatalTypeError(
      s"""malformed type: $pre is not a legal prefix for $denot because it contains abstract type member${if (absMembers.size == 1) "" else "s"} ${absMembers.mkString(", ")}""")

  class CyclicReference(val denot: SymDenotation)
    extends FatalTypeError(s"cyclic reference involving $denot") {
    def show(implicit ctx: Context) = s"cyclic reference involving ${denot.show}"
    printStackTrace()
  }

  class MergeError(msg: String) extends FatalTypeError(msg)

  // ----- Debug ---------------------------------------------------------

  var debugTrace = false

  val watchList = List[String](
  ) map (_.toTypeName)

  def isWatched(tp: Type) = tp match {
    case TypeRef(_, name) => watchList contains name
    case _ => false
  }

  // ----- Decorator implicits --------------------------------------------

  implicit def decorateTypeApplications(tpe: Type): TypeApplications = new TypeApplications(tpe)
}
