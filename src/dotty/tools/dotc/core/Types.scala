package dotty.tools.dotc
package core

import util.HashSet
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
import ast.tpd._, printing.Texts._
import transform.Erasure
import printing.Printer
import scala.util.hashing.{ MurmurHash3 => hashing }
import collection.mutable

object Types {

  /** A hash value indicating that the underlying type is not
   *  cached in uniques.
   */
  final val NotCached = 0

  /** An alternative value returned from `hash` if the
   *  computed hashCode would be `NotCached`.
   */
  private final val NotCachedAlt = Int.MinValue

  /** A value that indicates that the hash code is unknown
   */
  private final val HashUnknown = 1234

  /** An alternative value if computeHash would otherwise yield HashUnknown
   */
  private final val HashUnknownAlt = 4321

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
   *        |              |                +--- NoPrefix
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
   *                       +- ErrorType
   *                       +- WildcardType
   */
  abstract class Type extends DotClass with printing.Showable {

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
      case _ => false
    }

    /** Is this type an instance of the given class `cls`? */
    final def isClassType(cls: Symbol)(implicit ctx: Context): Boolean =
      dealias.typeSymbol == cls

    /** Is this type an instance of a non-bottom subclass of the given class `cls`? */
    final def derivesFrom(cls: Symbol)(implicit ctx: Context): Boolean =
      classSymbol.derivesFrom(cls)

    /** Is this an array type? */
    final def isArray(implicit ctx: Context): Boolean = isClassType(defn.ArrayClass)

   /** A type T is a legal prefix in a type selection T#A if
     *  T is stable or T contains no uninstantiated type variables.
     */
    final def isLegalPrefix(implicit ctx: Context): Boolean =
      isStable || memberNames(abstractTypeNameFilter).isEmpty

    /** Is this type guaranteed not to have `null` as a value?
     *  For the moment this is only true for modules, but it could
     *  be refined later.
     */
    final def isNotNull(implicit ctx: Context): Boolean =
      classSymbol is ModuleClass

    /** Is this type produced as a repair for an error? */
    final def isError(implicit ctx: Context): Boolean = thisInstance match {
      case ErrorType => true
      case tp => (tp.typeSymbol is Erroneous) || (tp.termSymbol is Erroneous)
    }

    /** Is some part of this type produced as a repair for an error? */
    final def isErroneous(implicit ctx: Context): Boolean = existsPart(_.isError)

    /** A type is volatile if its DNF contains an alternative of the form
     *  {P1, ..., Pn}, {N1, ..., Nk}, where the Pi are parent typerefs and the
     *  Nj are refinement names, and one the 4 following conditions is met:
     *
     *  1. At least two of the parents Pi are abstract types.
     *  2. One of the parents Pi is an abstract type, and one other type Pj,
     *     j != i has an abstract member which has the same name as an
     *     abstract member of the whole type.
     *  3. One of the parents Pi is an abstract type, and one of the refinement
     *     names Nj refers to an abstract member of the whole type.
     *  4. One of the parents Pi is an abstract type with a volatile upper bound.
     *
     *  Lazy values are not allowed to have volatile type, as otherwise
     *  unsoundness can result.
     */
    final def isVolatile(implicit ctx: Context): Boolean =
      ctx.isVolatile(this)

    /** Does the type carry an annotation that is an instance of `cls`? */
    final def hasAnnotation(cls: ClassSymbol)(implicit ctx: Context): Boolean = thisInstance match {
      case AnnotatedType(annot, tp) => annot.symbol == cls || tp.hasAnnotation(cls)
      case _ => false
    }

    /** Does this type occur as a part of type `that`? */
    final def occursIn(that: Type): Boolean = that.existsPart(this == _)

    def isRepeatedParam(implicit ctx: Context): Boolean =
      defn.RepeatedParamAliases contains typeSymbol

// ----- Higher-order combinators -----------------------------------

    /** Returns true if there is a part of this type that satisfies predicate `p`.
     */
    final def existsPart(p: Type => Boolean): Boolean =
      new ExistsAccumulator(p)(false, this)

    /** Returns true if all parts of this type satisfy predicate `p`.
     */
    final def forallParts(p: Type => Boolean): Boolean = !existsPart(!p(_))

    /** The parts of this type which are type or term refs */
    final def namedParts(implicit ctx: Context): Set[NamedType] =
      new PartsAccumulator().apply(Set(), this)

    /** Map function over elements of an AndType, rebuilding with & */
    def mapAnd(f: Type => Type)(implicit ctx: Context): Type = thisInstance match {
      case AndType(tp1, tp2) => tp1.mapAnd(f) & tp2.mapAnd(f)
      case tp => f(tp)
    }

    /** Map function over elements of an OrType, rebuilding with | */
    final def mapOr(f: Type => Type)(implicit ctx: Context): Type = thisInstance match {
      case OrType(tp1, tp2) => tp1.mapOr(f) | tp2.mapOr(f)
      case tp => f(tp)
    }

// ----- Associated symbols ----------------------------------------------

    /** The type symbol associated with the type */
    final def typeSymbol(implicit ctx: Context): Symbol = this match {
      case tp: TypeRef => tp.symbol
      case tp: ClassInfo => tp.cls
      case ThisType(cls) => cls
      case tp: TypeProxy => tp.underlying.typeSymbol
      case _ => NoSymbol
    }

    /** The least class or trait of which this type is a subtype, or
     *  NoSymbol if none exists (either because this type is not a
     *  value type, or because superclasses are ambiguous).
     */
    final def classSymbol(implicit ctx: Context): Symbol = this match {
      case tp: ClassInfo =>
        tp.cls
      case tp: TypeProxy =>
        tp.underlying.classSymbol
      case AndType(l, r) =>
        val lsym = l.classSymbol
        val rsym = r.classSymbol
        if (lsym.isSubClass(rsym)) lsym
        else if (rsym.isSubClass(lsym)) rsym
        else NoSymbol
      case OrType(l, r) =>
        val lsym = l.classSymbol
        val rsym = r.classSymbol
        if (lsym.isSubClass(rsym)) rsym
        else if (rsym.isSubClass(lsym)) lsym
        else NoSymbol
      case _ =>
        NoSymbol
    }

    /** The least (wrt <:<) set of class symbols of which this type is a subtype
     */
    final def classSymbols(implicit ctx: Context): Set[ClassSymbol] = this match {
      case tp: ClassInfo =>
        Set(tp.cls)
      case tp: TypeProxy =>
        tp.underlying.classSymbols
      case AndType(l, r) =>
        l.classSymbols | r.classSymbols
      case OrType(l, r) =>
        l.classSymbols & r.classSymbols
      case _ =>
        Set()
    }

    /** The term symbol associated with the type */
    final def termSymbol(implicit ctx: Context): Symbol = this match {
      case tp: TermRef => tp.symbol
      case tp: TypeProxy => tp.underlying.termSymbol
      case _ => NoSymbol
    }

    /** The base classes of this type as determined by ClassDenotation
     *  in linearization order, with the class itself as first element.
     *  Inherited by all type proxies. `Nil` for all other types.
     */
    final def baseClasses(implicit ctx: Context): List[ClassSymbol] = this match {
      case tp: TypeProxy =>
        tp.underlying.baseClasses
      case tp: ClassInfo =>
        tp.cls.baseClasses
      case _ => Nil
    }

    /** The type parameters of this type are:
     *  For a ClassInfo type, the type parameters of its class.
     *  For a typeref referring to a class, the type parameters of the class.
     *  For a typeref referring to an alias type, the type parameters of the aliased type.
     *  For a typeref referring to an abstract type with a HigherKindedXYZ bound, the
     *  type parameters of the HigherKinded class.
     *  For a refinement type, the type parameters of its parent, unless there's a
     *  refinement with the same name. Inherited by all other type proxies.
     *  For an intersection type A & B, the type parameters of its left operand, A.
     *  Empty list for all other types.
     */
    final def typeParams(implicit ctx: Context): List[TypeSymbol] = this match {
      case tp: ClassInfo =>
        tp.cls.typeParams
      case tp: TypeRef =>
        val tsym = tp.typeSymbol
        if (tsym.isClass) tsym.typeParams
        else if (tsym.isAliasType) tp.underlying.typeParams
        else tp.info.bounds.hi match {
          case AndType(hkBound, other) if defn.hkTraits contains hkBound.typeSymbol =>
            hkBound.typeSymbol.typeParams
          case _ =>
            Nil
        }
      case tp: RefinedType =>
        tp.parent.typeParams filterNot (_.name == tp.refinedName)
      case tp: TypeProxy =>
        tp.underlying.typeParams
      case tp: AndType =>
        tp.tp1.typeParams
      case _ =>
        Nil
    }

    def uninstantiatedTypeParams(implicit ctx: Context): List[TypeSymbol] =
      typeParams filter (tparam => member(tparam.name) == tparam)

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
    final def decl(name: Name)(implicit ctx: Context): Denotation =
      findDecl(name, EmptyFlags)

    /** A denotation containing the non-private declaration(s) in this type with the given name */
    final def nonPrivateDecl(name: Name)(implicit ctx: Context): Denotation =
      findDecl(name, Private)

    /** A denotation containing the declaration(s) in this type with the given
     *  name, as seen from prefix type `pre`. Declarations that have a flag
     *  in `excluded` are omitted.
     */
    final def findDecl(name: Name, excluded: FlagSet)(implicit ctx: Context): Denotation = this match {
      case tp: ClassInfo =>
        tp.decls.denotsNamed(name).filterExcluded(excluded).toDenot(NoPrefix)
      case tp: TypeProxy =>
        tp.underlying.findDecl(name, excluded)
    }

    /** The member of this type with the given name  */
    final def member(name: Name)(implicit ctx: Context): Denotation =
      findMember(name, this, EmptyFlags)

    /** The non-private member of this type with the given name. */
    final def nonPrivateMember(name: Name)(implicit ctx: Context): Denotation =
      findMember(name, this, Flags.Private)

    /** Find member of this type with given name and
     *  produce a denotation that contains the type of the member
     *  as seen from given prefix `pre`. Exclude all members that have
     *  flags in `excluded` from consideration.
     */
    final def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Denotation = this match {
      case tp: RefinedType =>
        val pdenot = tp.parent.findMember(name, pre, excluded)
        if (name eq tp.refinedName)
          pdenot & (new JointRefDenotation(NoSymbol, tp.refinedInfo.substThis(tp, pre), Period.allInRun(ctx.runId)), pre)
        else
          pdenot
      case tp: ThisType =>
        val d = tp.underlying.findMember(name, pre, excluded)
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
          tp.cls.symTypeRef.findMember(name, pre, excluded) orElse d
      case tp: TypeRef =>
        tp.denot.findMember(name, pre, excluded)
      case tp: TypeProxy =>
        tp.underlying.findMember(name, pre, excluded)
      case tp: ClassInfo =>
        tp.cls.findMember(name, pre, excluded)
      case AndType(l, r) =>
        l.findMember(name, pre, excluded) & (r.findMember(name, pre, excluded), pre)
      case OrType(l, r) =>
        l.findMember(name, pre, excluded) | (r.findMember(name, pre, excluded), pre)
      case NoType =>
        NoDenotation
    } /* !!! DEBUG ensuring { denot =>
      denot.alternatives forall (_.symbol.name == name)
    }*/

    /** The set of names of members of this type that pass the given name filter
     *  when seen as members of `pre`. More precisely, these are all
     *  of members `name` such that `keepOnly(pre, name)` is `true`.
     */
    final def memberNames(keepOnly: NameFilter, pre: Type = this)(implicit ctx: Context): Set[Name] = this match {
      case tp: ClassInfo =>
        tp.cls.memberNames(keepOnly) filter (keepOnly(pre, _))
      case tp: RefinedType =>
        val ns = tp.parent.memberNames(keepOnly, pre)
        if (keepOnly(pre, tp.refinedName)) ns + tp.refinedName else ns
      case tp: AndType =>
        tp.tp1.memberNames(keepOnly, pre) | tp.tp2.memberNames(keepOnly, pre)
      case tp: OrType =>
        tp.tp1.memberNames(keepOnly, pre) & tp.tp2.memberNames(keepOnly, pre)
      case tp: TypeProxy =>
        tp.underlying.memberNames(keepOnly, pre)
      case _ =>
        Set()
    }

    /** The set of names that denote an abstract member of this type
     *  which is also an abstract member of `pre`.
     */
    final def abstractMemberNames(pre: Type = this)(implicit ctx: Context): Set[Name] =
      memberNames(abstractTypeNameFilter, pre) |
      memberNames(abstractTermNameFilter, pre)

    /** The set of abstract term members of this type. */
    final def abstractTermMembers(implicit ctx: Context): Set[SingleDenotation] =
      memberNames(abstractTermNameFilter).flatMap(member(_).altsWith(_ is Deferred))

    /** The set of abstract type members of this type. */
    final def abstractTypeMembers(implicit ctx: Context): Set[SingleDenotation] =
      memberNames(abstractTypeNameFilter).map(member(_).asInstanceOf[SingleDenotation])

    /** The set of abstract members of this type. */
    final def abstractMembers(implicit ctx: Context): Set[SingleDenotation] =
      abstractTermMembers | abstractTypeMembers

    /** The set of type members of this type */
    final def typeMembers(implicit ctx: Context): Set[SingleDenotation] =
      memberNames(typeNameFilter).map(member(_).asInstanceOf[SingleDenotation])

    /** The set of implicit members of this type */
    final def implicitMembers(implicit ctx: Context): Set[TermRef] =
      memberNames(implicitFilter)
        .flatMap(name => member(name)
          .altsWith(_ is Implicit)
          .map(TermRef(this, name.asTermName).withDenot(_)))

    /** The info of `sym`, seen as a member of this type. */
    final def memberInfo(sym: Symbol)(implicit ctx: Context): Type =
      sym.info.asSeenFrom(this, sym.owner)

    /** This type seen as if it were the type of a member of prefix type `pre`
     *  declared in class `cls`.
     */
    final def asSeenFrom(pre: Type, cls: Symbol)(implicit ctx: Context): Type =
      if (!cls.membersNeedAsSeenFrom(pre)) this
      else ctx.asSeenFrom(this, pre, cls, null)

// ----- Subtype-related --------------------------------------------

    /** Is this type a subtype of that type? */
    final def <:<(that: Type)(implicit ctx: Context): Boolean =
      ctx.typeComparer.isSubType(this, that)

    /** Is this type the same as that type?
     *  This is the case iff `this <:< that` and `that <:< this`.
     */
    final def =:=(that: Type)(implicit ctx: Context): Boolean =
      ctx.typeComparer.isSameType(this, that)

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
      ctx.typeComparer.matchesType(
        this, that, alwaysMatchSimple = !ctx.phase.erasedTypes)

    /** The non-private symbol with given name in the given class that matches this type.
     *  @param inClass   The class containing the symbol's definition
     *  @param name      The name of the symbol we are looking for
     *  @param site      The base type from which member types are computed
     */
    def matchingTermSymbol(inClass: Symbol, name: Name, site: Type)(implicit ctx: Context): Symbol = {
      var denot = inClass.info.nonPrivateDecl(name)
      if (denot.isTerm) { // types of the same name always match
        if (denot.isOverloaded)
          denot = denot.atSignature(this.signature) // seems we need two kinds of signatures here
        if (!(site.memberInfo(denot.symbol) matches this))
          denot = NoDenotation
      }
      denot.symbol
    }

    /** The basetype of this type with given class symbol */
    final def baseType(base: Symbol)(implicit ctx: Context): Type = base.denot match {
      case classd: ClassDenotation => classd.baseTypeOf(this)
      case _ => NoType
    }

    def & (that: Type)(implicit ctx: Context): Type =
      ctx.glb(this, that)

    def | (that: Type)(implicit ctx: Context): Type =
      ctx.lub(this, that)

// ----- Unwrapping types -----------------------------------------------

    /** Map a TypeVar to either its instance if it is instantiated, or its origin,
     *  if not. Identity on all other types.
     */
    def thisInstance: Type = this

    /** Widen from singleton type to its underlying non-singleton
     *  base type by applying one or more `underlying` dereferences,
     *  Also go from => T to T.
     *  Identity for all other types. Example:
     *
     *  class Outer { class C ; val x: C }
     *  def o: Outer
     *  <o.x.type>.widen = o.C
     */
    final def widen(implicit ctx: Context): Type = this match {
      case tp: TermRef =>
      	if (tp.denot.isOverloaded) tp else tp.underlying.widen
      case tp: SingletonType => tp.underlying.widen
      case tp: TypeBounds => tp.hi.widen // needed?
      case tp: ExprType => tp.resultType.widen
      case _ => this
    }

    /** If this is an alias type, its alias, otherwise the type itself */
    final def dealias(implicit ctx: Context): Type = thisInstance match {
      case tp: TypeRef if (tp.symbol.isAliasType) => tp.info.bounds.hi
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
    final def unrefine: Type = thisInstance match {
      case tp @ RefinedType(tycon, _) => tycon.unrefine
      case tp => tp
    }

    /** Map references to Object to references to Any; needed for Java interop */
    final def objToAny(implicit ctx: Context) =
      if (isClassType(defn.ObjectClass) && !ctx.phase.erasedTypes) defn.AnyType else this

    /** If this is repeated parameter type, its underlying type,
     *  else the type itself.
     */
    def underlyingIfRepeated(implicit ctx: Context): Type =
      if (isRepeatedParam) dealias else this


// ----- Access to parts --------------------------------------------

    /** The normalized prefix of this type is:
     *  For an alias type, the normalized prefix of its alias
     *  For all other named type and class infos: the prefix.
     *  Inherited by all other type proxies.
     *  `NoType` for all other types.
     */
    final def normalizedPrefix(implicit ctx: Context): Type = this match {
      case tp: NamedType =>
        if (tp.symbol.isAliasType) tp.info.normalizedPrefix else tp.prefix
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

    /** The parameter types of a PolyType or MethodType, Empty list for others */
    final def paramTypess: List[List[Type]] = this match {
      case mt: MethodType => mt.paramTypes :: mt.resultType.paramTypess
      case pt: PolyType => pt.resultType.paramTypess
      case _ => Nil
    }
/* Not sure whether we'll need this
    final def firstParamTypes: List[Type] = this match {
      case mt: MethodType => mt.paramTypes
      case pt: PolyType => pt.firstParamTypes
      case _ => Nil
    }
*/
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
      case ci: ClassInfo => TypeAlias(ci.typeConstructor)
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

    /** The disjunctive normal form of this type.
     *  This collects a set of alternatives, each alternative consisting
     *  of a set of typerefs and a set of refinement names. Collected are
     *  all type refs reachable by following aliases and type proxies, and
     *  collecting the elements of conjunctions (&) and disjunctions (|).
     *  The set of refinement names in each alternative
     *  are the set of names in refinement types encountered during the collection.
     */
    final def DNF(implicit ctx: Context): Set[(Set[TypeRef], Set[Name])] = this match {
      case tp: TypeRef =>
        if (tp.symbol.isAliasType) tp.info.bounds.hi.DNF
        else Set((Set(tp), Set()))
      case RefinedType(parent, name) =>
        for ((ps, rs) <- parent.DNF) yield (ps, rs + name)
      case tp: TypeProxy =>
        tp.underlying.DNF
      case AndType(l, r) =>
        for ((lps, lrs) <- l.DNF; (rps, rrs) <- r.DNF)
        yield (lps | rps, lrs | rrs)
      case OrType(l, r) =>
        l.DNF | r.DNF
      case tp =>
        Set((Set(), Set()))
    }

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

// ----- Modeling type application --------------------------------

    /** Encode the type resulting from applying this type to given arguments */
    final def appliedTo(args: List[Type])(implicit ctx: Context): Type = {

      def recur(tp: Type, tparams: List[TypeSymbol], args: List[Type]): Type = args match {
        case arg :: args1 =>
          if (tparams.isEmpty) {
            println(s"applied type mismatch: $this $args, typeParams = $typeParams, tsym = ${this.typeSymbol.debugString}") // !!! DEBUG
            println(s"precomplete decls = ${typeSymbol.decls.toList.map(_.denot).mkString("\n  ")}")
          }
          val tparam = tparams.head
          val tp1 = RefinedType(tp, tparam.name, arg.toBounds(tparam))
          recur(tp1, tparams.tail, args1)
        case nil => tp
      }

      def safeTypeParams(tsym: Symbol) =
        if (tsym.isClass || !typeSymbol.isCompleting) typeParams
        else {
          ctx.warning("encountered F-bounded higher-kinded type parameters; assuming they are invariant")
          defn.hkTrait(args map Function.const(0)).typeParams
        }

      if (args.isEmpty) this
      else this match {
        case tp: TypeRef =>
          val tsym = tp.symbol
          if (tsym.isAliasType) tp.underlying.appliedTo(args)
          else recur(tp, safeTypeParams(tsym), args)
        case tp: TypeProxy =>
          tp.underlying.appliedTo(args)
        case AndType(l, r) =>
          l.appliedTo(args) & r
      }
    }

    final def appliedTo(arg: Type)(implicit ctx: Context): Type = appliedTo(arg :: Nil)
    final def appliedTo(arg1: Type, arg2: Type)(implicit ctx: Context): Type = appliedTo(arg1 :: arg2 :: Nil)

    /** Turn this type, which is used as an argument for
     *  type parameter `tparam`, into a TypeBounds RHS
     */
    final def toBounds(tparam: Symbol)(implicit ctx: Context): TypeBounds = {
      val v = tparam.variance
      if (v > 0) TypeBounds.upper(this)
      else if (v < 0) TypeBounds.lower(this)
      else TypeAlias(this)
    }

    /** If this is an encoding of a (partially) applied type, return its arguments,
     *  otherwise return Nil
     */
    final def typeArgs(implicit ctx: Context): List[Type] = {
      var tparams: List[TypeSymbol] = null
      def recur(tp: Type, refineCount: Int): mutable.ListBuffer[Type] = tp match {
        case tp @ RefinedType(tycon, name) =>
          val buf = recur(tycon, refineCount + 1)
          if (buf == null) null
          else {
            if (tparams == null) tparams = tycon.typeParams
            if (buf.size < tparams.length) {
              val tparam = tparams(buf.size)
              if (name == tparam.name) buf += tp.refinedInfo.argType(tparam)
              else null
            } else null
          }
        case _ =>
          if (refineCount == 0) null
          else new mutable.ListBuffer[Type]
      }
      val buf = recur(thisInstance, 0)
      if (buf == null) Nil else buf.toList
    }

    /** If this is the image of a type argument to type parameter `tparam`,
     *  recover the type argument, otherwise NoType.
     */
    final def argType(tparam: Symbol)(implicit ctx: Context): Type = this match {
      case TypeBounds(lo, hi) =>
        val v = tparam.variance
        if (v > 0 && lo.isClassType(defn.NothingClass)) hi
        else if (v < 0 && hi.isClassType(defn.AnyClass)) lo
        else if (v == 0 && (lo eq hi)) lo
        else NoType
      case _ =>
        NoType
    }

    /** If this type is of the normalized form Array[...[Array[T]...]
     *  return the number of Array wrappers and T.
     *  Otherwise return 0 and the type itself
     */
    final def splitArray(implicit ctx: Context): (Int, Type) = {
      def recur(n: Int, tp: Type): (Int, Type) = tp match {
        case RefinedType(tycon, _) if tycon.isArray =>
          tp.typeArgs match {
            case arg :: Nil => recur(n + 1, arg)
            case _ => (n, tp)
          }
        case _ =>
          (n, tp)
      }
      recur(0, thisInstance)
    }

    /** Given a type alias
     *
     *      type T[boundSyms] = p.C[targs]
     *
     *  produce its equivalent right hand side RHS that makes no reference to the bound
     *  symbols on the left hand side. I.e. the type alias can be replaced by
     *
     *      type T = RHS
     *
     *  It is required that `C` is a class and that every bound symbol in `boundSyms` appears
     *  as an argument in `targs`. If these requirements are not met an error is
     *  signalled by calling the parameter `error`.
     *
     *  The rewriting replaces bound symbols by references to the
     *  parameters of class C. Example:
     *
     *  Say we have:
     *
     *     class Triple[type T1, type T2, type T3]
     *     type A[X] = Triple[(X, X), X, String]
     *
     *  Then this is rewritable, as `X` appears as second type argument to `Triple`.
     *  Occurrences of `X` are rewritten to `this.T2` and the whole definition becomes:
     *
     *     type A = Triple { type T1 = (this.T2, this.T2); type T3 = String }
     *
     *  If the RHS is an intersection type A & B, we Lambda abstract on A instead and
     *  then recombine with & B.
     */
    def LambdaAbstract(boundSyms: List[Symbol])(error: (String, Position) => Unit)(implicit ctx: Context): Type = this match {
      case AndType(l, r) =>
        AndType(l.LambdaAbstract(boundSyms)(error), r)
      case _ =>
        val cls = typeSymbol
        if (!cls.isClass)
          error("right-hand side of parameterized alias type must refer to a class", cls.pos)

        val correspondingParamName: Map[Symbol, TypeName] = {
          for { (tparam, targ: TypeRef) <- cls.typeParams zip typeArgs
              if boundSyms contains targ.symbol
          } yield targ.symbol -> tparam.name
        }.toMap

        val correspondingNames = correspondingParamName.values.toSet

        def replacements(rt: RefinedType): List[Type] =
          for (sym <- boundSyms) yield {
            correspondingParamName get sym match {
              case Some(name) =>
                TypeRef(RefinedThis(rt), name)
              case None =>
                error(s"parameter $sym of type alias does not appear as type argument of the aliased $cls", sym.pos)
                defn.AnyType
            }
          }

        def rewrite(tp: Type): Type = tp match {
          case tp @ RefinedType(parent, name: TypeName) =>
            if (correspondingNames contains name) rewrite(parent)
            else RefinedType(
              rewrite(parent),
              name,
              rt => tp.refinedInfo.subst(boundSyms, replacements(rt)))
          case tp =>
            tp
        }

        rewrite(this)
    }

// ----- misc -----------------------------------------------------------

    /** The signature of this type. This is by default NotAMethod,
     *  but is overridden for PolyTypes, MethodTypes, and TermRefWithSignature types.
     *  (the reason why we deviate from the "final-method-with-pattern-match-in-base-class"
     *   pattern is that method signatures use caching, so encapsulation
     *   is improved using an OO scheme).
     */
    def signature(implicit ctx: Context): Signature = NotAMethod

    def toText(printer: Printer): Text = printer.toText(this)

    def varianceOf(tp: Type): FlagSet = ???

// ----- hashing ------------------------------------------------------

    /** customized hash code of this type.
     *  NotCached for uncached types. Cached types
     *  compute hash and use it as the type's hashCode.
     */
    def hash: Int

    protected def hashSeed = getClass.hashCode

    private def finishHash(hashCode: Int, arity: Int): Int = {
      val h = hashing.finalizeHash(hashCode, arity)
      if (h == NotCached) NotCachedAlt else h
    }

    private def finishHash(seed: Int, arity: Int, tp: Type): Int = {
      val elemHash = tp.hash
      if (elemHash == NotCached) return NotCached
      finishHash(hashing.mix(seed, elemHash), arity + 1)
    }

    private def finishHash(seed: Int, arity: Int, tp1: Type, tp2: Type): Int = {
      val elemHash = tp1.hash
      if (elemHash == NotCached) return NotCached
      finishHash(hashing.mix(seed, elemHash), arity + 1, tp2)
    }

    private def finishHash(seed: Int, arity: Int, tps: List[Type]): Int = {
      var h = seed
      var xs = tps
      var len = arity
      while (xs.nonEmpty) {
        val elemHash = xs.head.hash
        if (elemHash == NotCached) return NotCached
        h = hashing.mix(h, elemHash)
        xs = xs.tail
        len += 1
      }
      finishHash(h, len)
    }

    private def finishHash(seed: Int, arity: Int, tp: Type, tps: List[Type]): Int = {
      val elemHash = tp.hash
      if (elemHash == NotCached) return NotCached
      finishHash(hashing.mix(seed, elemHash), arity + 1, tps)
    }

    protected def doHash(x: Any): Int =
      finishHash(hashing.mix(hashSeed, x.hashCode), 1)

    protected def doHash(tp: Type): Int =
      finishHash(hashSeed, 0, tp)

    protected def doHash(x1: Any, tp2: Type): Int =
      finishHash(hashing.mix(hashSeed, x1.hashCode), 1, tp2)

    protected def doHash(tp1: Type, tp2: Type): Int =
      finishHash(hashSeed, 0, tp1, tp2)

    protected def doHash(x1: Any, tp2: Type, tp3: Type): Int =
      finishHash(hashing.mix(hashSeed, x1.hashCode), 1, tp2, tp3)

    protected def doHash(tp1: Type, tps2: List[Type]): Int =
      finishHash(hashSeed, 0, tp1, tps2)

    protected def doHash(x1: Any, tp2: Type, tps3: List[Type]): Int =
      finishHash(hashing.mix(hashSeed, x1.hashCode), 1, tp2, tps3)

  } // end Type

  /** A marker trait for cached types */
  trait CachedType extends Type

  def unique[T <: Type](tp: T)(implicit ctx: Context): T = {
    if (tp.hash == NotCached) tp
    else ctx.uniques.findEntryOrUpdate(tp).asInstanceOf[T]
  } /* !!! DEBUG
  ensuring (
    result => tp.toString == result.toString || {
      println(s"cache mismatch; tp = $tp, cached = $result")
      false
    }
  )
 */

// ----- Type categories ----------------------------------------------

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
    private[this] var _hash = HashUnknown
    final def hash = {
      if (_hash == HashUnknown) {
        _hash = computeHash
        if (_hash == HashUnknown) _hash = HashUnknownAlt
      }
      _hash
    }
    override final def hashCode =
      if (hash == NotCached) System.identityHashCode(this) else hash
    def computeHash: Int
  }

  /**  Instances of this class are cached and are proxies. */
  abstract class CachedProxyType extends TypeProxy with CachedType {
    private[this] var _hash = HashUnknown
    final def hash = {
      if (_hash == HashUnknown) {
        _hash = computeHash
        if (_hash == HashUnknown) _hash = HashUnknownAlt
      }
      _hash
    }
    override final def hashCode =
      if (hash == NotCached) System.identityHashCode(this) else hash
    def computeHash: Int
  }

  /**  Instances of this class are uncached and are not proxies. */
  abstract class UncachedGroundType extends Type {
    final def hash = NotCached
  }

  /**  Instances of this class are uncached and are proxies. */
  abstract class UncachedProxyType extends TypeProxy {
    final def hash = NotCached
  }

  /** A marker trait for types that apply only to type symbols */
  trait TypeType extends Type

  /** A marker trait for types that apply only to term symbols */
  trait TermType extends Type

  /** A marker trait for types that can be types of values */
  trait ValueType extends TermType

  /** A marker trait for types that are guaranteed to contain only a
   *  single non-null value (they might contain null in addition).
   */
  trait SingletonType extends TypeProxy with ValueType

  /** A marker trait for types that bind other types that refer to them.
   *  Instances are: PolyType, MethodType, RefinedType.
   */
  trait BindingType extends Type

// --- NamedTypes ------------------------------------------------------------------

  /** A NamedType of the form Prefix # name */
  abstract class NamedType extends CachedProxyType with ValueType {

    val prefix: Type
    val name: Name

    private[this] var lastDenotation: Denotation = null

    /** The denotation currently denoted by this type */
    def denot(implicit ctx: Context): Denotation = {
      val validPeriods =
        if (lastDenotation != null) lastDenotation.validFor else Nowhere
      val thisPeriod = ctx.period
      if (!(validPeriods contains thisPeriod)) {
        lastDenotation =
          if (validPeriods.runId == thisPeriod.runId) {
            lastDenotation.current
          } else {
            val d = loadDenot
/* need to do elsewhere as it leads to a cycle in subtyping here.
            if (d.exists && !d.symbol.isAliasType && !prefix.isLegalPrefix) {
              val ex = new MalformedType(prefix, d, prefix.memberNames(abstractTypeNameFilter))
              if (ctx.checkPrefix) {
                ctx.printCreationTrace()
                throw ex
              } else ctx.log(ex.getMessage)
            }
*/
            if (d.exists || ctx.phaseId == FirstPhaseId)
              d
            else // name has changed; try load in earlier phase and make current
              denot(ctx.fresh.withPhase(ctx.phaseId - 1)).current
          }
      }
      lastDenotation
    }

    private[dotc] final def withDenot(denot: Denotation): this.type = {
      lastDenotation = denot
      this
    }

    protected def loadDenot(implicit ctx: Context) = prefix.member(name)

    def isType = name.isTypeName
    def isTerm = name.isTermName

    def symbol(implicit ctx: Context): Symbol = denot.symbol
    def info(implicit ctx: Context): Type = denot.info

    override def underlying(implicit ctx: Context): Type = info

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
      else if (ctx.pendingUnderlying(this))
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

    def derivedNamedType(prefix: Type)(implicit ctx: Context): NamedType =
      if (prefix eq this.prefix) this
      else newLikeThis(prefix)

    /** Create a NamedType of the same kind as this type, if possible,
     *  but with a new prefix. For HasFixedSym instances another such
     *  instance is only created if the symbol's owner is a base class of
     *  the new prefix. If that is not the case, we fall back to a
     *  NamedType or in the case of a TermRef, NamedType with signature.
     */
    protected def newLikeThis(prefix: Type)(implicit ctx: Context): NamedType =
      NamedType(prefix, name)

    override def computeHash = doHash(name, prefix)

    override def equals(that: Any) = that match {
      case that: HasFixedSym => false
      case that: TermRefWithSignature => false
      case that: NamedType =>
        this.prefix == that.prefix &&
        this.name == that.name
      case _ =>
        false
    }
  }

  abstract case class TermRef(override val prefix: Type, name: TermName) extends NamedType with SingletonType

  abstract case class TypeRef(override val prefix: Type, name: TypeName) extends NamedType

  trait HasFixedSym extends NamedType {
    protected val fixedSym: Symbol
    override def symbol(implicit ctx: Context): Symbol = fixedSym
    override def loadDenot(implicit ctx: Context) = {
      val denot = fixedSym.denot
      val owner = denot.owner
      if (owner.isTerm) denot else denot.asSeenFrom(prefix)
    }
    override def equals(that: Any) = that match {
      case that: HasFixedSym =>
        this.prefix == that.prefix &&
        this.fixedSym == that.fixedSym
      case _ =>
        false
    }
    override def computeHash = doHash(fixedSym, prefix)
  }

  final class TermRefBySym(prefix: Type, name: TermName, val fixedSym: TermSymbol)
    extends TermRef(prefix, name) with HasFixedSym

  final class TermRefWithSignature(prefix: Type, name: TermName, val sig: Signature) extends TermRef(prefix, name) {
    override def signature(implicit ctx: Context) = sig
    override def loadDenot(implicit ctx: Context): Denotation =
      super.loadDenot.atSignature(sig)
    override def newLikeThis(prefix: Type)(implicit ctx: Context): TermRefWithSignature =
      TermRef.withSig(prefix, name, sig)
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

  final class TypeRefBySym(prefix: Type, name: TypeName, val fixedSym: TypeSymbol)
    extends TypeRef(prefix, name) with HasFixedSym

  final class CachedTermRef(prefix: Type, name: TermName) extends TermRef(prefix, name)
  final class CachedTypeRef(prefix: Type, name: TypeName) extends TypeRef(prefix, name)

  object NamedType {
    def apply(prefix: Type, name: Name)(implicit ctx: Context) =
      if (name.isTermName) TermRef(prefix, name.asTermName)
      else TypeRef(prefix, name.asTypeName)
   def withSym(prefix: Type, sym: Symbol)(implicit ctx: Context) =
      if (sym.isTerm) TermRef.withSym(prefix, sym.asTerm)
      else TypeRef.withSym(prefix, sym.asType)
  }

  object TermRef {
    def apply(prefix: Type, name: TermName)(implicit ctx: Context): TermRef =
      unique(new CachedTermRef(prefix, name))
    def withSym(prefix: Type, name: TermName, sym: TermSymbol)(implicit ctx: Context): TermRefBySym =
      unique(new TermRefBySym(prefix, name, sym))
    def withSym(prefix: Type, sym: TermSymbol)(implicit ctx: Context): TermRefBySym =
      withSym(prefix, sym.name, sym)
    def withSig(prefix: Type, name: TermName, sig: Signature)(implicit ctx: Context): TermRefWithSignature =
      unique(new TermRefWithSignature(prefix, name, sig))
  }

  object TypeRef {
    def apply(prefix: Type, name: TypeName)(implicit ctx: Context): TypeRef =
      unique(new CachedTypeRef(prefix, name))
    def withSym(prefix: Type, name: TypeName, sym: TypeSymbol)(implicit ctx: Context): TypeRefBySym =
      unique(new TypeRefBySym(prefix, name, sym))
    def withSym(prefix: Type, sym: TypeSymbol)(implicit ctx: Context): TypeRefBySym =
      withSym(prefix, sym.name, sym)
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
  abstract case class RefinedType(parent: Type, refinedName: Name)(infoFn: RefinedType => Type)
    extends CachedProxyType with BindingType with ValueType {

    val refinedInfo: Type = infoFn(this)

    override def underlying(implicit ctx: Context) = parent

    def derivedRefinedType(parent: Type, refinedName: Name, refinedInfo: Type)(implicit ctx: Context): RefinedType = {
      def originalName = parent.typeParams.apply(refinedName.hkParamIndex).name
      if ((parent eq this.parent) && (refinedName eq this.refinedName) && (refinedInfo eq this.refinedInfo))
        this
      else if (refinedName.isHkParamName &&
               refinedName.hkParamIndex < typeParams.length &&
               originalName != refinedName)
        derivedRefinedType(parent, originalName, refinedInfo)
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
    override def toString = s"RefinedType($parent, $refinedName, $refinedInfo | hash = $hashCode)"
  }

  class CachedRefinedType(parent: Type, refinedName: Name, infoFn: RefinedType => Type) extends RefinedType(parent, refinedName)(infoFn)

  object RefinedType {
    def make(parent: Type, names: List[Name], infoFns: List[RefinedType => Type])(implicit ctx: Context): Type =
      if (names.isEmpty) parent
      else make(RefinedType(parent, names.head, infoFns.head), names.tail, infoFns.tail)

    def apply(parent: Type, name: Name, infoFn: RefinedType => Type)(implicit ctx: Context): RefinedType =
      unique(new CachedRefinedType(parent, name, infoFn))

    def apply(parent: Type, name: Name, info: Type)(implicit ctx: Context): RefinedType =
      apply(parent, name, scala.Function.const(info): (RefinedType => Type))
  }

  // --- AndType/OrType ---------------------------------------------------------------

  abstract case class AndType(tp1: Type, tp2: Type) extends CachedGroundType with ValueType {
    assert(tp1.isInstanceOf[TermType] && tp2.isInstanceOf[TermType], s"$tp1 & $tp2")

    type This <: AndType

    def derivedAndType(tp1: Type, tp2: Type)(implicit ctx: Context) =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else AndType(tp1, tp2)

    override def computeHash = doHash(tp1, tp2)
  }

  final class CachedAndType(tp1: Type, tp2: Type) extends AndType(tp1, tp2)

  object AndType {
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context) =
      unique(new CachedAndType(tp1, tp2))
  }

  abstract case class OrType(tp1: Type, tp2: Type) extends CachedGroundType with ValueType {
    def derivedOrType(tp1: Type, tp2: Type)(implicit ctx: Context) =
      if ((tp1 eq this.tp1) && (tp2 eq this.tp2)) this
      else OrType(tp1, tp2)

    override def computeHash = doHash(tp1, tp2)
  }

  final class CachedOrType(tp1: Type, tp2: Type) extends OrType(tp1, tp2)

  object OrType {
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context) =
      unique(new CachedOrType(tp1, tp2))
  }

  // ----- Method types: MethodType/ExprType/PolyType/MethodParam/PolyParam ---------------

  // Note: method types are cached whereas poly types are not.
  // The reason is that most poly types are cyclic via poly params,
  // and therefore two different poly types would never be equal.

  abstract case class MethodType(paramNames: List[TermName], paramTypes: List[Type])
      (resultTypeExp: MethodType => Type)
    extends CachedGroundType with BindingType with TermType {

    override val resultType = resultTypeExp(this)
    def isJava = false
    def isImplicit = false

    lazy val isDependent = resultType existsPart {
      case MethodParam(mt, _) => mt eq this
      case _ => false
    }

    private[this] var _signature: Signature = _
    private[this] var signatureRunId: Int = NoRunId

    override def signature(implicit ctx: Context): Signature = {
      if (ctx.runId != signatureRunId) {
        _signature = computeSignature
        signatureRunId = ctx.runId
      }
      _signature
    }

    private def computeSignature(implicit ctx: Context): Signature = {
      val followSig = resultType match {
        case rtp: MethodType => rtp.signature
        case _ => Nil
      }
      (paramTypes map Erasure.paramSignature) ++ followSig
    }

    def derivedMethodType(paramNames: List[TermName], paramTypes: List[Type], restpe: Type)(implicit ctx: Context) =
      if ((paramNames eq this.paramNames) && (paramTypes eq this.paramTypes) && (restpe eq this.resultType)) this
      else {
        val restpeExpr = (x: MethodType) => restpe.subst(this, x)
        if (isJava) JavaMethodType(paramNames, paramTypes)(restpeExpr)
        else if (isImplicit) ImplicitMethodType(paramNames, paramTypes)(restpeExpr)
        else MethodType(paramNames, paramTypes)(restpeExpr)
      }

    def instantiate(argTypes: => List[Type])(implicit ctx: Context): Type =
      if (isDependent) new InstMethodMap(this, argTypes) apply resultType
      else resultType

 /* probably won't be needed
    private var _isVarArgs: Boolean = _
    private var knownVarArgs: Boolean = false

    def isVarArgs(implicit ctx: Context) = {
      if (!knownVarArgs) {
        _isVarArgs = paramTypes.nonEmpty && paramTypes.last.isRepeatedParam
        knownVarArgs = true
      }
      _isVarArgs
    }
*/
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
    override def computeHash = super.computeHash + 1
    override protected def prefixString = "JavaMethodType"
  }

  final class ImplicitMethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)
    extends MethodType(paramNames, paramTypes)(resultTypeExp) {
    override def isImplicit = true
    override def equals(that: Any) = super.equals(that) && that.isInstanceOf[ImplicitMethodType]
    override def computeHash = super.computeHash + 2
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
      extends CachedProxyType with TermType {
    override def underlying(implicit ctx: Context): Type = resultType
    override def signature(implicit ctx: Context): Signature = Nil
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
      extends UncachedGroundType with BindingType with TermType {
    val paramBounds = paramBoundsExp(this)
    override val resultType = resultTypeExp(this)

    override def signature(implicit ctx: Context) = resultType.signature

    def instantiate(argTypes: List[Type])(implicit ctx: Context): Type =
      new InstPolyMap(this, argTypes) apply resultType

    def instantiateBounds(argTypes: List[Type])(implicit ctx: Context): List[TypeBounds] =
      paramBounds.mapConserve(new InstPolyMap(this, argTypes).apply(_).bounds)

    def derivedPolyType(paramNames: List[TypeName], paramBounds: List[TypeBounds], restpe: Type)(implicit ctx: Context) =
      if ((paramNames eq this.paramNames) && (paramBounds eq this.paramBounds) && (restpe eq this.resultType)) this
      else copy(paramNames, paramBounds, restpe)

    def copy(paramNames: List[TypeName], paramBounds: List[TypeBounds], restpe: Type)(implicit ctx: Context) =
      PolyType(paramNames)(
          x => paramBounds mapConserve (_.subst(this, x).bounds),
          x => restpe.subst(this, x))

    // need to override hashCode and equals to be object identity
    // because paramNames by itself is not discriminatory enough
    override def hashCode = System.identityHashCode(this)
    override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]

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

  abstract class BoundType extends UncachedProxyType with ValueType {
    type BT <: BindingType
    def binder: BT
    def copy(bt: BT): Type
  }

  abstract class ParamType extends BoundType {
    def paramNum: Int
  }

  case class MethodParam(binder: MethodType, paramNum: Int) extends ParamType with SingletonType {
    type BT = MethodType
    override def underlying(implicit ctx: Context): Type = binder.paramTypes(paramNum)
    def copy(bt: BT) = MethodParam(bt, paramNum)

    // need to customize hashCode and equals to prevent infinite recursion for dep meth types.
    override def hashCode = doHash(System.identityHashCode(binder) + paramNum)
    override def equals(that: Any) = that match {
      case that: MethodParam =>
        (this.binder eq that.binder) &&
        this.paramNum == that.paramNum
      case _ =>
        false
    }

    override def toString = s"MethodParam(${binder.paramNames(paramNum)})"
  }

  case class PolyParam(binder: PolyType, paramNum: Int) extends ParamType {
    type BT = PolyType
    def copy(bt: BT) = PolyParam(bt, paramNum)
    override def underlying(implicit ctx: Context): Type = binder.paramBounds(paramNum)
    // no customized hashCode/equals needed because cycle is broken in PolyType
    override def toString = s"PolyParam(${binder.paramNames(paramNum)})"
  }

  case class RefinedThis(binder: RefinedType) extends BoundType with SingletonType {
    type BT = RefinedType
    override def underlying(implicit ctx: Context) = binder.parent
    def copy(bt: BT) = RefinedThis(bt)
    // need to customize hashCode and equals to prevent infinite recursion for
    // refinements that refer to the refinement type via this
    override def hashCode = doHash(System.identityHashCode(binder))
    override def equals(that: Any) = that match {
      case that: RefinedThis => this.binder eq that.binder
      case _ => false
    }
    override def toString = s"RefinedThis(${binder.hashCode})"
  }

  final case class TypeVar(origin: PolyParam) extends UncachedProxyType with ValueType {
    private var inst: Type = NoType
    def isInstantiated = inst ne NoType
    def instantiateWith(tp: Type) = inst = tp
    override def thisInstance = if (isInstantiated) inst else origin
    override def underlying(implicit ctx: Context): Type = thisInstance
    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
    override def toString = thisInstance.toString
  }

  // ------ ClassInfo, Type Bounds ------------------------------------------------------------

  /** The info of a class during a period, roughly
   *  @param prefix       The prefix on which parents, decls, and selfType need to be rebased.
   *  @param cls          The class symbol.
   *  @param classParents The parent types of this class.
   *                      These are all normalized to be TypeRefs by moving any refinements
   *                      to be member definitions of the class itself.
   *  @param decls        The symbols defined directly in this class.
   *  @param optSelfType  The type of `this` in this class, if explicitly given, NoType otherwise.
   */
  abstract case class ClassInfo(
      prefix: Type,
      cls: ClassSymbol,
      classParents: List[TypeRef],
      decls: Scope,
      optSelfType: Type) extends CachedGroundType with TypeType {

    def selfType(implicit ctx: Context): Type =
      if (optSelfType.exists) optSelfType else cls.typeConstructor

    def rebase(tp: Type)(implicit ctx: Context): Type =
      if ((prefix eq cls.owner.thisType) || !cls.owner.isClass) tp
      else tp.substThis(cls.owner.asClass, prefix)

    private var tyconCache: Type = null

    def typeConstructor(implicit ctx: Context): Type = {
      def clsDenot = if (prefix eq cls.owner.thisType) cls.denot else cls.denot.copySymDenotation(info = this)
      if (tyconCache == null)
        tyconCache =
          if ((cls is PackageClass) || cls.owner.isTerm) TypeRef.withSym(prefix, cls)
          else TypeRef(prefix, cls.name).withDenot(clsDenot)
      tyconCache
    }

    // cached because baseType needs parents
    private var parentsCache: List[TypeRef] = null

    override def parents(implicit ctx: Context): List[TypeRef] = {
      if (parentsCache == null)
        parentsCache = classParents.mapConserve(rebase(_).asInstanceOf[TypeRef])
      parentsCache
    }

    def derivedClassInfo(prefix: Type)(implicit ctx: Context) =
      if (prefix eq this.prefix) this
      else ClassInfo(prefix, cls, classParents, decls, optSelfType)

    def derivedClassInfo(prefix: Type, classParents: List[TypeRef], optSelfType: Type)(implicit ctx: Context) =
      if ((prefix eq this.prefix) && (classParents eq this.classParents) && (optSelfType eq this.optSelfType)) this
      else ClassInfo(prefix, cls, classParents, decls, optSelfType)

    override def computeHash = doHash(cls, prefix)
  }

  final class CachedClassInfo(prefix: Type, cls: ClassSymbol, classParents: List[TypeRef], decls: Scope, optSelfType: Type)
    extends ClassInfo(prefix, cls, classParents, decls, optSelfType)

  object ClassInfo {
    def apply(prefix: Type, cls: ClassSymbol, classParents: List[TypeRef], decls: Scope, optSelfType: Type = NoType)(implicit ctx: Context) =
      unique(new CachedClassInfo(prefix, cls, classParents, decls, optSelfType))
  }

  /** Type bounds >: lo <: hi */
  abstract case class TypeBounds(lo: Type, hi: Type) extends CachedProxyType with TypeType {

    assert(lo.isInstanceOf[TermType], lo+" "+lo.getClass)
    assert(hi.isInstanceOf[TermType], hi+" "+hi.getClass)

    override def underlying(implicit ctx: Context): Type = hi

    def derivedTypeBounds(lo: Type, hi: Type)(implicit ctx: Context) =
      if ((lo eq this.lo) && (hi eq this.hi)) this
      else TypeBounds(lo, hi)

    def contains(tp: Type)(implicit ctx: Context) = lo <:< tp && tp <:< hi

    def &(that: TypeBounds)(implicit ctx: Context): TypeBounds =
      derivedTypeBounds(this.lo | that.lo, this.hi & that.hi)

    def | (that: TypeBounds)(implicit ctx: Context): TypeBounds =
      derivedTypeBounds(this.lo & that.lo, this.hi | that.hi)

    override def & (that: Type)(implicit ctx: Context) = that match {
      case that: TypeBounds => this & that
      case that: ClassInfo => this & that.bounds
    }

    override def | (that: Type)(implicit ctx: Context) = that match {
      case that: TypeBounds => this | that
    }

    def map(f: Type => Type)(implicit ctx: Context): TypeBounds =
      TypeBounds(f(lo), f(hi))

    /** Given a higher-kinded abstract type
     *
     *    type T[boundSyms] >: L <: H
     *
     *  produce its equivalent bounds L',R that make no reference to the bound
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
     *    reference `this._$hk$i`.
     *  - L' = substBoundSyms(L), H' = substBoundSyms(H)
     *
     *  Example:
     *
     *      type T[X <: F[X]] <: Traversable[X, T]
     *
     *  is rewritten to:
     *
     *      type T <: HigherKindedP { type _$hk$0 <: F[$_hk$0] } & Traversable[_$hk$0, T]
     *
     *  @see Definitions.hkTrait
     */
    def higherKinded(boundSyms: List[Symbol])(implicit ctx: Context) = {
      val parent = defn.hkTrait(boundSyms map (_.variance)).typeConstructor
      val hkParamNames = boundSyms.indices.toList map tpnme.higherKindedParamName
      def substBoundSyms(tp: Type)(rt: RefinedType): Type =
        tp.subst(boundSyms, hkParamNames map (TypeRef(RefinedThis(rt), _)))
      val hkParamInfoFns: List[RefinedType => Type] =
        for (bsym <- boundSyms) yield substBoundSyms(bsym.info)_
      val hkBound = RefinedType.make(parent, hkParamNames, hkParamInfoFns).asInstanceOf[RefinedType]
      TypeBounds(substBoundSyms(lo)(hkBound), AndType(hkBound, substBoundSyms(hi)(hkBound)))
    }

    override def computeHash = doHash(lo, hi)

    override def toString =
      if (lo eq hi) s"TypeAlias($lo)" else s"TypeBounds($lo, $hi)"
  }

  final class CachedTypeBounds(lo: Type, hi: Type) extends TypeBounds(lo, hi)

  object TypeBounds {
    def empty(implicit ctx: Context) = apply(defn.NothingType, defn.AnyType)
    def upper(hi: Type)(implicit ctx: Context) = apply(defn.NothingType, hi)
    def lower(lo: Type)(implicit ctx: Context) = apply(lo, defn.AnyType)
    def apply(lo: Type, hi: Type)(implicit ctx: Context) =
      unique(new CachedTypeBounds(lo, hi))
  }

  object TypeAlias {
    def apply(tp: Type)(implicit ctx: Context) = TypeBounds(tp, tp)
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
  abstract case class WildcardType(optBounds: Type) extends CachedGroundType {
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
   *   - has a single abstract method
   *   - can be instantiated without arguments or with just () as argument.
   */
  object SAMType {
    def isInstantiatable(tp: Type)(implicit ctx: Context): Boolean = tp match {
      case tp: TypeRef =>
        isInstantiatable(tp.info)
      case tp: ClassInfo =>
        def zeroParams(tp: Type): Boolean = tp match {
          case pt: PolyType => zeroParams(pt)
          case mt: MethodType => mt.paramTypess.isEmpty && !mt.resultType.isInstanceOf[MethodType]
          case et: ExprType => true
          case _ => false
        }
        val noParamsNeeded = (tp.cls is Trait) || zeroParams(tp.cls.primaryConstructor.info)
        val selfTypeFeasible = tp.typeConstructor <:< tp.selfType
        noParamsNeeded && selfTypeFeasible
      case tp: RefinedType =>
        isInstantiatable(tp.underlying)
      case tp: TypeVar =>
        isInstantiatable(tp.thisInstance)
      case _ =>
        false
    }
    def unapply(tp: Type)(implicit ctx: Context): Option[(SingleDenotation, List[Type], Type)] =
      if (isInstantiatable(tp)) {
        val absMems = tp.abstractTermMembers
        if (absMems.size == 1)
          absMems.head.info match {
            case mt: MethodType if !mt.isDependent => Some((absMems.head, mt.paramTypes, mt.resultType))
            case _=> None
          }
        else None
      }
      else None
  }

  // ----- TypeMaps --------------------------------------------------------------------

  abstract class TypeMap(implicit ctx: Context) extends (Type => Type) { thisMap =>
    def apply(tp: Type): Type

    /** Map this function over given type */
    def mapOver(tp: Type): Type = tp match {
      case tp: NamedType =>
        tp.derivedNamedType(this(tp.prefix))

      case _: ThisType
         | _: BoundType => tp

      case tp: RefinedType =>
        tp.derivedRefinedType(this(tp.parent), tp.refinedName, this(tp.refinedInfo))

      case tp @ MethodType(pnames, ptypes) =>
        tp.derivedMethodType(pnames, ptypes mapConserve this, this(tp.resultType))

      case tp @ ExprType(restpe) =>
        tp.derivedExprType(this(restpe))

      case tp @ PolyType(pnames) =>
        tp.derivedPolyType(
          pnames, tp.paramBounds.mapConserve(apply(_).bounds), this(tp.resultType))

      case tp @ SuperType(thistp, supertp) =>
        tp.derivedSuperType(this(thistp), this(supertp))

      case tp @ TypeBounds(lo, hi) =>
        if (lo eq hi) {
          val lo1 = this(lo)
          tp.derivedTypeBounds(lo1, lo1)
        } else {
          tp.derivedTypeBounds(this(lo), this(hi))
        }

      case tp @ ClassInfo(prefix, _, _, _, _) =>
        tp.derivedClassInfo(this(prefix))

      case tp @ AnnotatedType(annot, underlying) =>
        tp.derivedAnnotatedType(mapOver(annot), this(underlying))

      case tp @ TypeVar(_) =>
        apply(tp.thisInstance)

      case tp @ WildcardType =>
        tp.derivedWildcardType(mapOver(tp.optBounds))

      case _ =>
        tp
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
      new TreeMapper(this).apply(tree)

    def andThen(f: Type => Type): TypeMap = new TypeMap {
      def apply(tp: Type) = f(thisMap(tp))
    }
  }

  object IdentityTypeMap extends TypeMap()(NoContext) {
    def apply(tp: Type) = tp
  }

  class InstMethodMap(mt: MethodType, argtypes: List[Type])(implicit ctx: Context) extends TypeMap {
    def apply(tp: Type) = tp match {
      case MethodParam(`mt`, n) => argtypes(n)
      case _ => mapOver(tp)
    }
  }

  class InstPolyMap(pt: PolyType, argtypes: List[Type])(implicit ctx: Context) extends TypeMap {
    def apply(tp: Type) = tp match {
      case PolyParam(`pt`, n) => argtypes(n)
      case _ => mapOver(tp)
    }
  }

  /** Approximate occurrences of paremter types and uninstantiated typevars
   *  by wildcard types
   */
  class WildApprox(implicit ctx: Context) extends TypeMap {
    override def apply(tp: Type) = tp match {
      case PolyParam(pt, pnum) =>
        WildcardType(apply(pt.paramBounds(pnum)).bounds)
      case MethodParam(mt, pnum) =>
        WildcardType(TypeBounds.upper(apply(mt.paramTypes(pnum))))
      case tp: TypeVar =>
        apply(tp.underlying)
      case _ =>
        mapOver(tp)
    }
  }

  // ----- TypeAccumulators ----------------------------------------------------

  abstract class TypeAccumulator[T] extends ((T, Type) => T) {
    def apply(x: T, tp: Type): T

    protected def apply(x: T, annot: Annotation): T = x // don't go into annotations

    def foldOver(x: T, tp: Type): T = tp match {
      case tp: NamedType =>
        this(x, tp.prefix)

      case _: ThisType
         | _: BoundType => x

      case tp: RefinedType =>
        this(this(x, tp.parent), tp.refinedInfo)

      case tp @ MethodType(pnames, ptypes) =>
        this((x /: ptypes)(this), tp.resultType)

      case ExprType(restpe) =>
        this(x, restpe)

      case tp @ PolyType(pnames) =>
        this((x /: tp.paramBounds)(this), tp.resultType)

      case SuperType(thistp, supertp) =>
        this(this(x, thistp), supertp)

      case TypeBounds(lo, hi) =>
        if (lo eq hi) this(x, lo)
        else this(this(x, lo), hi)

      case AnnotatedType(annot, underlying) =>
        this(this(x, annot), underlying)

      case tp: TypeVar =>
        foldOver(x, tp.thisInstance)

      case tp: WildcardType =>
        foldOver(x, tp.optBounds)

      case _ => x
    }
  }

  class ExistsAccumulator(p: Type => Boolean) extends TypeAccumulator[Boolean] {
    def apply(x: Boolean, tp: Type) = x || p(tp) || foldOver(x, tp)
  }

  class PartsAccumulator(implicit ctx: Context) extends TypeAccumulator[Set[NamedType]] {
    def apply(x: Set[NamedType], tp: Type): Set[NamedType] = tp match {
      case tp: NamedType =>
        foldOver(x + tp, tp)
      case tp: ThisType =>
        apply(x, tp.underlying)
      case tp: TypeVar =>
        apply(x, tp.underlying)
      case _ =>
        foldOver(x, tp)
    }
  }

  //   ----- Name Filters --------------------------------------------------

  /** A name filter selects or discards a member name of a type `pre`.
   *  To enable efficient caching, name filters have to satisfy the
   *  following invariant: If `keep` is a name filter, and `pre` has
   *  class `C` as a base class, then
   *
   *    keep(pre, name) => keep(C.this, name)
   */
  abstract class NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean
  }

  /** A filter for names of abstract types of a given type */
  object abstractTypeNameFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean =
      name.isTypeName && ((pre member name).symbol is Deferred)
  }

  /** A filter for names of deferred term definitions of a given type */
  object abstractTermNameFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean =
      name.isTermName && (pre member name).hasAltWith(_ is Deferred)
  }

  object typeNameFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean = name.isTypeName
  }

  object takeAllFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean = true
  }

  object implicitFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean =
      (pre member name).hasAltWith(_ is Implicit)
  }

  // ----- Exceptions -------------------------------------------------------------

  class TypeError(msg: String) extends Exception(msg)
  class FatalTypeError(msg: String) extends TypeError(msg)

  class MalformedType(pre: Type, denot: Denotation, absMembers: Set[Name])
    extends FatalTypeError(
      s"""malformed type: $pre is not a legal prefix for $denot because it contains abstract type member${if (absMembers.size == 1) "" else "s"} ${absMembers.mkString(", ")}"""
         .stripMargin)

  class CyclicReference(val denot: SymDenotation)
    extends FatalTypeError(s"cyclic reference involving $denot")

  // ----- Debug ---------------------------------------------------------

  var debugTrace = false
}
