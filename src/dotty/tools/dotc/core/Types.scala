package dotty.tools.dotc
package core

import util.HashSet
import Symbols._
import TypeComparers._
import Flags._
import Names._
import Scopes._
import Constants._
import Contexts._
import Annotations._
import SymDenotations._
import Denotations._
import Periods._
import TypedTrees.tpd._, TypedTrees.TreeMapper
import transform.Erasure
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
  abstract class Type extends DotClass {

    /** The type symbol associated with the type */
    final def typeSymbol(implicit ctx: Context): Symbol = this match {
      case tp: TypeRef => tp.symbol
      case tp: ClassInfo => tp.cls
      case tp: TypeProxy => tp.underlying.typeSymbol
      case _ => NoSymbol
    }

    /** The term symbol associated with the type */
    final def termSymbol(implicit ctx: Context): Symbol = this match {
      case tp: TermRef => tp.symbol
      case tp: TypeProxy => tp.underlying.termSymbol
      case _ => NoSymbol
    }

    /** Does this type denote a stable reference (i.e. singleton type)? */
    final def isStable(implicit ctx: Context): Boolean = this match {
      case tp: TermRef => tp.prefix.isStable && tp.termSymbol.isStable
      case _: SingletonType => true
      case _ => false
    }

    /** A type T is a legal prefix in a type selection T#A if
     *  T is stable or T contains no uninstantiated type variables.
     */
    final def isLegalPrefix(implicit ctx: Context): Boolean =
      isStable || abstractTypeNames(this).isEmpty

    /** The set of names that denote an abstract type member of this type
     *  which is also an abstract type member of `pre`
     */
    final def abstractTypeNames(pre: Type)(implicit ctx: Context): Set[Name] =
      memberNames(pre, abstractTypeNameFilter)
    final def abstractTypeNames(implicit ctx: Context): Set[Name] =
      abstractTypeNames(this)

    /** The set of names that denote an abstract term member of this type
     *  which is also an abstract term member of `pre`
     */
    final def abstractTermNames(pre: Type)(implicit ctx: Context): Set[Name] =
      memberNames(pre, abstractTermNameFilter)
    final def abstractTermNames(implicit ctx: Context): Set[Name] =
      abstractTermNames(this)

    /** The set of names that denote an abstract member of this type
     *  which is also an abstract member of `pre`
     */
    final def abstractMemberNames(pre: Type)(implicit ctx: Context): Set[Name] =
      abstractTypeNames(pre) | abstractTermNames(pre)
    final def abstractMemberNames(implicit ctx: Context): Set[Name] =
      abstractMemberNames(this)

    final def abstractTermMembers(pre: Type)(implicit ctx: Context): Set[SingleDenotation] =
      abstractTermNames.flatMap(name =>
        pre.member(name).altsWith(_ is Deferred))
    final def abstractTermMembers(implicit ctx: Context): Set[SingleDenotation] =
      abstractTermMembers(this)

    /** The set of names of members of this type that pass the given name filter
     *  when seen as members of `pre`. More precisely, these are all
     *  of members `name` such that `keepOnly(pre, name)` is `true`.
     */
    final def memberNames(pre: Type, keepOnly: NameFilter)(implicit ctx: Context): Set[Name] = this match {
      case tp: ClassInfo =>
        tp.cls.memberNames(keepOnly) filter (keepOnly(pre, _))
      case tp: RefinedType =>
        var ns = tp.parent.memberNames(pre, keepOnly)
        if (keepOnly(pre, tp.name)) ns += tp.name
        ns
      case tp: AndType =>
        tp.tp1.memberNames(pre, keepOnly) | tp.tp2.memberNames(pre, keepOnly)
      case tp: OrType =>
        tp.tp1.memberNames(pre, keepOnly) & tp.tp2.memberNames(pre, keepOnly)
      case tp: TypeProxy =>
        tp.underlying.memberNames(pre, keepOnly)
      case _ =>
        Set()
    }

    /** Is this type a value type */
    final def isValueType: Boolean = this match {
      case NoPrefix
         | NoType
         | WildcardType
         | _: TypeBounds
         | _: MethodType
         | _: PolyType
         | _: ExprType
         | _: ClassInfo => false
      case _ => true
    }

    /** Is this type a TypeBounds instance, with lower and upper bounds
     *  that are not identical?
     */
    final def isRealTypeBounds: Boolean = this match {
      case tp: TypeBounds => tp.lo ne tp.hi
      case _ => false
    }

    /** Is this type a TypeBounds instance, with lower and upper bounds
     *  that are identical?
     */
    final def isAliasTypeBounds: Boolean = this match {
      case tp: TypeBounds => tp.lo eq tp.hi
      case _ => false
    }

    /** This type seen as a TypeBounds */
    final def bounds(implicit ctx: Context): TypeBounds = this match {
      case tp: TypeBounds => tp
      case _ => TypeBounds(this, this)
    }

    /** A type is volatile if it has an underlying type of the
     *  form P1 with ... with Pn { decls } (where n may be 1 or decls may
     *  be empty), one of the parent types Pi is an abstract type, and
     *  either decls or a different parent Pj, j != i, contributes
     *  an abstract member.
     *
     *  A type contributes an abstract member if it has an abstract member which
     *  is also a member of the whole refined type. A scope `decls` contributes
     *  an abstract member if it has an abstract definition which is also
     *  a member of the whole type.
     *
     *  Lazy values are not allowed to have volatile type, as otherwise
     *  unsoundness can result.
     */
    final def isVolatile(implicit ctx: Context): Boolean =
      ctx.isVolatile(this)

    /** Is this type guaranteed not to have `null` as a value? */
    final def isNotNull: Boolean = false

    /** Is this type produced as a repair for an error? */
    final def isError(implicit ctx: Context): Boolean =
      (typeSymbol is Erroneous) || (termSymbol is Erroneous)

    /** Is some part of this type produced as a repair for an error? */
    final def isErroneous(implicit ctx: Context): Boolean = exists(_.isError)

    /** Returns true if there is a part of this type that satisfies predicate `p`.
     */
    final def exists(p: Type => Boolean): Boolean =
      new ExistsAccumulator(p)(false, this)

    final def forall(p: Type => Boolean): Boolean = !exists(!p(_))

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

    /** Substitute all occurrences symbols in `from` by references to corresponding symbols in `to`
     */
    final def substSym(from: List[Symbol], to: List[Symbol])(implicit ctx: Context): Type =
      ctx.substSym(this, from, to, null)

    /** For a ClassInfo type, its parents,
     *  Inherited by all type proxies. Empty for all other types.
     *  Overwritten in ClassInfo, where parents is cached.
     */
    def parents(implicit ctx: Context): List[TypeRef] = this match {
      case tp: TypeProxy =>
        tp.underlying.parents
      case _ => List()
    }

    /** The elements of an AndType or OrType */
    def factors(implicit ctx: Context): List[Type] = this match {
      case tp: AndType =>
        def components(tp: Type): List[Type] = tp match {
          case AndType(tp1, tp2) => components(tp1) ++ components(tp2)
          case _ => List(tp)
        }
        components(tp)
      case tp: OrType =>
        def components(tp: Type): List[Type] = tp match {
          case OrType(tp1, tp2) => components(tp1) ++ components(tp2)
          case _ => List(tp)
        }
        components(tp)
      case _ => List()
    }

    /** If this is an alias type, its alias, otherwise the type itself */
    def dealias(implicit ctx: Context): Type = this match {
      case tp: TypeRef =>
        val info = tp.info
        if (info.isAliasTypeBounds) info.dealias else this
      case _ =>
        this
    }

    /** The parameter types of a PolyType or MethodType, Empty list for others */
    def paramTypess: List[List[Type]] = this match {
      case mt: MethodType => mt.paramTypes :: mt.resultType.paramTypess
      case pt: PolyType => pt.paramTypess
      case _ => Nil
    }

    /** The resultType of a PolyType, MethodType, or ExprType, the type itself for others */
    def resultType: Type = this match {
      case et: ExprType => et.resultType
      case mt: MethodType => mt.resultType
      case pt: PolyType => pt.resultType
      case _ => this
    }

    /** Map function over elements of an AndType, rebuilding with & */
    def mapAnd(f: Type => Type)(implicit ctx: Context): Type = this match {
      case AndType(tp1, tp2) => tp1.mapAnd(f) & tp2.mapAnd(f)
      case _ => f(this)
    }

    /** Map function over elements of an OrType, rebuilding with | */
    def mapOr(f: Type => Type)(implicit ctx: Context): Type = this match {
      case OrType(tp1, tp2) => tp1.mapOr(f) | tp2.mapOr(f)
      case _ => f(this)
    }

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

    /** The declaration of this type with given name */
    final def decl(name: Name)(implicit ctx: Context): Denotation =
      findDecl(name, this, EmptyFlags)

    /** The non-private declaration of this type with given name */
    final def nonPrivateDecl(name: Name)(implicit ctx: Context): Denotation =
      findDecl(name, this, Flags.Private)

    /** The non-private class member declaration of this type with given name */
    final def findDecl(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Denotation = this match {
      case tp: ClassInfo =>
        tp.decls
          .denotsNamed(name)
          .filterAccessibleFrom(pre)
          .filterExcluded(excluded)
          .asSeenFrom(pre)
          .toDenot
      case tp: TypeProxy =>
        tp.underlying.findDecl(name, pre, excluded)
    }

    /** The member of this type with given name  */
    final def member(name: Name)(implicit ctx: Context): Denotation =
      findMember(name, this, EmptyFlags)

    /** The non-private member of this type with given name */
    final def nonPrivateMember(name: Name)(implicit ctx: Context): Denotation =
      findMember(name, this, Flags.Private)

    /** Find member of this type with given name and
     *  produce a denotation that contains the type of the member
     *  as seen from given prefix `pre`. Exclude all members with one
     *  of the flags in `excluded` from consideration.
     */
    final def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Denotation = this match {
      case tp: RefinedType =>
        val pdenot = tp.parent.findMember(name, pre, excluded)
        if (name eq tp.name)
          pdenot & new JointRefDenotation(NoSymbol, tp.info.substThis(tp, pre), Period.allInRun(ctx.runId))
        else
          pdenot
      case tp: TypeProxy =>
        tp.underlying.findMember(name, pre, excluded)
      case tp: ClassInfo =>
        val cls = tp.cls
        val candidates = cls.membersNamed(name)
        val results = candidates
          .filterAccessibleFrom(pre)
          .filterExcluded(excluded)
          .asSeenFrom(pre)
        if (results.exists) results.toDenot
        else new ErrorDenotation // todo: refine
      case tp: AndType =>
        tp.tp1.findMember(name, pre, excluded) & tp.tp2.findMember(name, pre, excluded)
      case tp: OrType =>
        (tp.tp1.findMember(name, pre, excluded) | tp.tp2.findMember(name, pre, excluded))(pre)
    }

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
     *    - Or both types are (possibly nullary) method types with equivalent type parameter types
     *      and matching result types
     *    - Or both types are equivalent
     *    - Or phase.erasedTypes is false and both types are neither method nor
     *      poly types.
     */
    def matches(that: Type)(implicit ctx: Context): Boolean =
      ctx.typeComparer.matchesType(this, that, !ctx.phase.erasedTypes)

    /** Does this type match that type
     *
     */

    /** The info of `denot`, seen as a member of this type. */
//    final def memberInfo(denot: SymDenotation)(implicit ctx: Context): Type = {
//      denot.info.asSeenFrom(this, denot.owner)
//    }

    /** The info of `sym`, seen as a member of this type. */
    final def memberInfo(sym: Symbol)(implicit ctx: Context): Type = {
      sym.info.asSeenFrom(this, sym.owner)
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
    final def widen(implicit ctx: Context): Type = this match {
      case tp: SingletonType => tp.underlying.widen
      case tp: ExprType => tp.underlying.widen
      case _ => this
    }

    /** Widen from constant type to its underlying non-constant
     *  base type.
     */
    final def deconst(implicit ctx: Context): Type = this match {
      case tp: ConstantType => tp.value.tpe
      case _ => this
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

    final def asSeenFrom(pre: Type, cls: Symbol)(implicit ctx: Context): Type =
      if ((cls is PackageClass) ||
        ctx.erasedTypes && cls != defn.ArrayClass ||
        (pre eq cls.thisType)) this
      else ctx.asSeenFrom(this, pre, cls, null)

    /** The signature of this type. This is by default NotAMethod,
     *  but is overridden for PolyTypes, MethodTypes, and TermRefWithSignature types.
     *  (the reason why we deviate from the "final-method-with-pattern-match-in-base-class"
     *   pattern is that method signatures use caching, so encapsulation
     *   is improved using an OO scheme).
     */
    def signature(implicit ctx: Context): Signature = NotAMethod

    final def baseType(base: Symbol)(implicit ctx: Context): Type = base.denot match {
      case classd: ClassDenotation => classd.baseTypeOf(this)
      case _ => NoType
    }

    /** The type parameters of this type are:
     *  For a ClassInfo type, the type parameters of its denotation.
     *  Inherited by type proxies.
     *  Empty list for all other types.
     */
    final def typeParams(implicit ctx: Context): List[TypeSymbol] = this match {
      case tp: ClassInfo =>
        tp.cls.typeParams
      case tp: TypeProxy =>
        tp.underlying.typeParams
      case _ => Nil
    }

    /** Encode the type resulting from applying this type to given arguments */
    final def appliedTo(args: List[Type])(implicit ctx: Context): Type = {
      def recur(tp: Type, tparams: List[TypeSymbol], args: List[Type]): Type = args match {
        case arg :: args1 =>
          val tparam = tparams.head
          val tp1 = RefinedType(tp, tparam.name, arg.toRHS(tparam))
          recur(tp1, tparams.tail, args1)
        case nil => tp
      }
      if (args.isEmpty) this else recur(this, typeParams, args)
    }

    final def appliedTo(arg: Type)(implicit ctx: Context): Type = appliedTo(arg :: Nil)
    final def appliedTo(arg1: Type, arg2: Type)(implicit ctx: Context): Type = appliedTo(arg1 :: arg2 :: Nil)

    final def objToAny(implicit ctx: Context) =
      if (typeSymbol == defn.ObjectClass && !ctx.phase.erasedTypes) defn.AnyType else this

    /** If this type equals `tycon applyToArgs args`, for some
     *  non-refinement type `tycon` and (possibly partial) type arguments
     *  `args`, return a pair consisting of `tycon` and `args`.
     *  Otherwise return the dealiased type itself and `Nil`.
     */
    final def splitArgs(implicit ctx: Context): (Type, List[Type]) = {
      def recur(tp: Type, nparams: Int): (Type, List[Type]) = tp.dealias match {
        case tp @ RefinedType(parent, name) =>
          def fail = (NoType, Nil)
          if (nparams >= 0) {
            val result @ (tycon, args) = recur(parent, nparams - 1)
            if (tycon != NoType) {
              val tparam = tycon.typeParams.apply(nparams)
              if (tparam.name == name) {
                (tycon, args :+ tp.info.argType(tparam))
              } else fail
            } else fail
          } else fail
        case tp =>
          (tp, Nil)
      }
      val result @ (tycon, args) = recur(this, typeParams.length)
      if (tycon != NoType) result else (this, Nil)
    }

    final def splitArgsCompletely(implicit ctx: Context): (Type, List[Type]) = {
      val result @ (tycon, args) = splitArgs
      if (args.length == tycon.typeParams.length) result else (NoType, Nil)
    }

    /** If this is an encoding of an applied type, return its arguments,
     *  otherwise return Nil
     */
    def typeArgs(implicit ctx: Context): List[Type] = splitArgs._2

    /** If this type is of the normalized form Array[...[Array[T]...]
     *  return the number of Array wrappers and T.
     *  Otherwise return 0 and the type itself
     */
    final def splitArray(implicit ctx: Context): (Int, Type) = {
      def recur(n: Int, tp: Type): (Int, Type) = tp.splitArgs match {
        case (arrayType, arg :: Nil) if arrayType == defn.ArrayType =>
          recur(n + 1, arg)
        case (tp, Nil) =>
          (n, tp)
      }
      recur(0, this)
    }

    /** Turn this type into a TypeBounds RHS */
    final def toRHS(tparam: Symbol)(implicit ctx: Context): TypeBounds = {
      val v = tparam.variance
      if (v > 0) TypeBounds.upper(this)
      else if (v < 0) TypeBounds.lower(this)
      else TypeAlias(this)
    }

    /** If this is the image of a type argument, recover the type argument,
     *  otherwise NoType.
     */
    final def argType(tparam: Symbol)(implicit ctx: Context): Type = this match {
      case TypeBounds(lo, hi) =>
        val v = tparam.variance
        if (v > 0 && lo.typeSymbol == defn.NothingClass) hi
        else if (v < 0 && hi.typeSymbol == defn.AnyClass) lo
        else if (v == 0 && (lo eq hi)) lo
        else NoType
      case _ =>
        NoType
    }

    final def isWrong: Boolean = !exists // !!! needed?
    final def exists: Boolean = true

    final def &(that: Type)(implicit ctx: Context): Type =
      ctx.glb(this, that)

    def |(that: Type)(implicit ctx: Context): Type =
      ctx.lub(this, that)

    def show(implicit ctx: Context): String = ctx.show(this, Printers.GlobalPrec)

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
  }

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
    final val hash = computeHash
    override final def hashCode = hash
    def computeHash: Int
  }

  /**  Instances of this class are cached and are proxies. */
  abstract class CachedProxyType extends TypeProxy with CachedType {
    final val hash = computeHash
    override final def hashCode = hash
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

  /** A marker trait for types that are guaranteed to contain only a
   *  single non-null value (they might contain null in addition).
   */
  trait SingletonType extends TypeProxy

  /** A marker trait for types that apply only to type symbols */
  trait TypeType extends Type

  // --- NamedTypes ------------------------------------------------------------------

  /** A NamedType of the form Prefix # name
   */
  abstract class NamedType extends CachedProxyType {

    val prefix: Type
    val name: Name

    private[this] var lastDenotation: Denotation = null

    /** The denotation currently denoted by this type */
    def denot(implicit ctx: Context): Denotation = {
      val validPeriods =
        if (lastDenotation != null) lastDenotation.validFor else Nowhere
      if (!(validPeriods contains ctx.period)) {
        val thisPeriod = ctx.period
        lastDenotation =
          if (validPeriods.runId == thisPeriod.runId) {
            lastDenotation.current
          } else {
            val d = loadDenot
            if (d.exists || ctx.phaseId == FirstPhaseId) {
              val checkPrefix =
                d.info.isRealTypeBounds || d.symbol.isClass
              if (checkPrefix && !prefix.isLegalPrefix)
                throw new MalformedType(prefix, d.symbol)
              d
            } else {// name has changed; try load in earlier phase and make current
              denot(ctx.fresh.withPhase(ctx.phaseId - 1)).current
            }
          }
      }
      lastDenotation
    }

    protected def loadDenot(implicit ctx: Context) = prefix.member(name)

    def isType = name.isTypeName
    def isTerm = name.isTermName

    def symbol(implicit ctx: Context): Symbol = denot.symbol
    def info(implicit ctx: Context): Type = denot.info

    override def underlying(implicit ctx: Context): Type = info

    def derivedNamedType(prefix: Type)(implicit ctx: Context): Type =
      if (prefix eq this.prefix) this
      else newLikeThis(prefix)

    /** Create a NamedType of the same kind as this type, if possible,
     *  but with a new prefix. For HasFixedSym instances another such
     *  instance is only created if the symbol's owner is a base class of
     *  the new prefix. If that is not the case, we fall back to a
     *  NamedType or in the case of a TermRef, NamedType with signature.
     */
    protected def newLikeThis(prefix: Type)(implicit ctx: Context): Type =
      NamedType(prefix, name)

    override def computeHash = doHash(name, prefix)
  }

  abstract case class TermRef(override val prefix: Type, name: TermName) extends NamedType with SingletonType

  abstract case class TypeRef(override val prefix: Type, name: TypeName) extends NamedType

  trait HasFixedSym extends NamedType {
    protected val fixedSym: Symbol
    override def symbol(implicit ctx: Context): Symbol = fixedSym
    override def loadDenot(implicit ctx: Context) = {
      val denot = fixedSym.denot
      val owner = denot.owner
      if (owner.isTerm) denot else denot.asSeenFrom(prefix).toDenot
    }
  }

  final class TermRefBySym(prefix: Type, val fixedSym: TermSymbol)(initctx: Context)
    extends TermRef(prefix, fixedSym.name(initctx).asTermName) with HasFixedSym {
    override def newLikeThis(prefix: Type)(implicit ctx: Context): TermRef =
      if (prefix.baseType(fixedSym.owner).exists) TermRef(prefix, fixedSym)
      else TermRef(prefix, name, fixedSym.signature)
 }

  final class TermRefWithSignature(prefix: Type, name: TermName, sig: Signature) extends TermRef(prefix, name) {
    override def signature(implicit ctx: Context) = sig
    override def computeHash = doHash((name, sig), prefix)
    override def loadDenot(implicit ctx: Context): Denotation =
      super.loadDenot.atSignature(sig)
    override def newLikeThis(prefix: Type)(implicit ctx: Context): TermRefWithSignature =
      TermRef(prefix, name, sig)
   }

  final class TypeRefBySym(prefix: Type, val fixedSym: TypeSymbol)(initctx: Context)
    extends TypeRef(prefix, fixedSym.name(initctx).asTypeName) with HasFixedSym {
    override def newLikeThis(prefix: Type)(implicit ctx: Context): TypeRef =
      if (prefix.baseType(fixedSym.owner).exists) TypeRef(prefix, fixedSym)
      else TypeRef(prefix, name)
 }

  final class CachedTermRef(prefix: Type, name: TermName) extends TermRef(prefix, name)
  final class CachedTypeRef(prefix: Type, name: TypeName) extends TypeRef(prefix, name)

  object NamedType {
    def apply(prefix: Type, name: Name)(implicit ctx: Context) =
      if (name.isTermName) TermRef(prefix, name.asTermName)
      else TypeRef(prefix, name.asTypeName)
   def apply(prefix: Type, sym: Symbol)(implicit ctx: Context) =
      if (sym.isTerm) TermRef(prefix, sym.asTerm)
      else TypeRef(prefix, sym.asType)
  }

  object TermRef {
    def apply(prefix: Type, name: TermName)(implicit ctx: Context) =
      unique(new CachedTermRef(prefix, name))
    def apply(prefix: Type, sym: TermSymbol)(implicit ctx: Context) =
      unique(new TermRefBySym(prefix, sym)(ctx))
    def apply(prefix: Type, name: TermName, sig: Signature)(implicit ctx: Context) =
      unique(new TermRefWithSignature(prefix, name, sig))
  }

  object TypeRef {
    def apply(prefix: Type, name: TypeName)(implicit ctx: Context) =
      unique(new CachedTypeRef(prefix, name))
    def apply(prefix: Type, sym: TypeSymbol)(implicit ctx: Context) =
      unique(new TypeRefBySym(prefix, sym)(ctx))
  }

  // --- Other SingletonTypes: ThisType/SuperType/ConstantType ---------------------------

  abstract case class ThisType(cls: ClassSymbol) extends CachedProxyType with SingletonType {
    override def underlying(implicit ctx: Context) = cls.classInfo.selfType
    override def computeHash = doHash(cls)
  }

  final class CachedThisType(cls: ClassSymbol) extends ThisType(cls)

  object ThisType {
    def apply(cls: ClassSymbol)(implicit ctx: Context) = {
      assert(!(cls is PackageClass) || cls.isRoot)
      unique(new CachedThisType(cls))
    }
  }

  abstract case class SuperType(thistpe: Type, supertpe: Type) extends CachedProxyType with SingletonType {
    override def underlying(implicit ctx: Context) = supertpe
    def derivedSuperType(thistp: Type, supertp: Type)(implicit ctx: Context) =
      if ((thistp eq thistpe) && (supertp eq supertpe)) this
      else SuperType(thistp, supertp)
    override def computeHash = doHash(thistpe, supertpe)
  }

  final class CachedSuperType(thistpe: Type, supertpe: Type) extends SuperType(thistpe, supertpe)

  object SuperType {
    def apply(thistpe: Type, supertpe: Type)(implicit ctx: Context) =
      unique(new CachedSuperType(thistpe, supertpe))
  }

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

  abstract case class RefinedType(parent: Type, name: Name)(infof: RefinedType => Type) extends CachedProxyType with BindingType {

    val info: Type = infof(this)

    override def underlying(implicit ctx: Context) = parent

    def derivedRefinedType(parent: Type, name: Name, info: Type)(implicit ctx: Context): RefinedType =
      if ((parent eq this.parent) && (name eq this.name) && (info eq this.info)) this
      else RefinedType(parent, name, rt => info.substThis(this, RefinedThis(rt)))

    override def computeHash = doHash(name, info, parent)
  }

  class CachedRefinedType(parent: Type, name: Name, infof: RefinedType => Type) extends RefinedType(parent, name)(infof)

  object RefinedType {
    def make(parent: Type, names: List[Name], infofs: List[RefinedType => Type])(implicit ctx: Context): Type =
      if (names.isEmpty) parent
      else make(RefinedType(parent, names.head, infofs.head), names.tail, infofs.tail)

    def apply(parent: Type, name: Name, infof: RefinedType => Type)(implicit ctx: Context): RefinedType =
      unique(new CachedRefinedType(parent, name, infof))

    def apply(parent: Type, name: Name, info: Type)(implicit ctx: Context): RefinedType =
      apply(parent, name, scala.Function const info: (RefinedType => Type))
  }

  // --- AndType/OrType ---------------------------------------------------------------

  abstract case class AndType(tp1: Type, tp2: Type) extends CachedGroundType {

    type This <: AndType

    def derivedAndType(t1: Type, t2: Type)(implicit ctx: Context) =
      if ((t1 eq tp1) && (t2 eq tp2)) this
      else AndType(t1, t2)

    override def computeHash = doHash(tp1, tp2)
  }

  final class CachedAndType(tp1: Type, tp2: Type) extends AndType(tp1, tp2)

  object AndType {
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context) =
      unique(new CachedAndType(tp1, tp2))
  }

  abstract case class OrType(tp1: Type, tp2: Type) extends CachedGroundType {
    def derivedOrType(t1: Type, t2: Type)(implicit ctx: Context) =
      if ((t1 eq tp1) && (t2 eq tp2)) this
      else OrType(t1, t2)

    override def computeHash = doHash(tp1, tp2)
  }

  final class CachedOrType(tp1: Type, tp2: Type) extends OrType(tp1, tp2)

  object OrType {
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context) =
      unique(new CachedOrType(tp1, tp2))
  }

  // ----- Method types: MethodType/ExprType/PolyType/MethodParam/PolyParam ---------------

  trait BindingType extends Type

  // Note: method types are cached whereas poly types are not.
  // The reason is that most poly types are cyclic via poly params,
  // and therefore two different poly types would never be equal.

  abstract case class MethodType(paramNames: List[TermName], paramTypes: List[Type])
      (resultTypeExp: MethodType => Type) extends CachedGroundType with BindingType {
    override lazy val resultType = resultTypeExp(this)
    def isJava = false
    def isImplicit = false

    lazy val isDependent = resultType exists {
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

    def instantiate(argTypes: List[Type])(implicit ctx: Context): Type =
      if (isDependent) new InstMethodMap(this, argTypes) apply resultType
      else resultType

    override def computeHash = doHash(paramNames, resultType, paramTypes)
  }

  final class CachedMethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)
    extends MethodType(paramNames, paramTypes)(resultTypeExp)

  final class JavaMethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)
    extends MethodType(paramNames, paramTypes)(resultTypeExp) {
    override def isJava = true
  }

  final class ImplicitMethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)
    extends MethodType(paramNames, paramTypes)(resultTypeExp) {
    override def isImplicit = true
  }

  abstract class GenericMethodType {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context): MethodType
    def apply(paramNames: List[TermName], paramTypes: List[Type], resultType: Type)(implicit ctx: Context): MethodType =
      apply(paramNames, paramTypes)(_ => resultType)
    def fromSymbols(params: List[Symbol], resultType: Type)(implicit ctx: Context) = {
      def transResult(mt: MethodType) =
        resultType.subst(params, (0 until params.length).toList map (MethodParam(mt, _)))
      apply(params map (_.name.asTermName), params map (_.info))(transResult _)
    }
  }

  object MethodType extends GenericMethodType {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context) =
      unique(new CachedMethodType(paramNames, paramTypes)(resultTypeExp))
  }

  object JavaMethodType extends GenericMethodType {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context) =
      unique(new JavaMethodType(paramNames, paramTypes)(resultTypeExp))
  }

  object ImplicitMethodType extends GenericMethodType {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context) =
      unique(new ImplicitMethodType(paramNames, paramTypes)(resultTypeExp))
  }

  abstract case class ExprType(override val resultType: Type) extends CachedProxyType {
    override def underlying(implicit ctx: Context): Type = resultType
    override def signature(implicit ctx: Context): Signature = Nil
    def derivedExprType(rt: Type)(implicit ctx: Context) =
      if (rt eq resultType) this else ExprType(rt)
    override def computeHash = doHash(resultType)
  }

  final class CachedExprType(resultType: Type) extends ExprType(resultType)

  object ExprType {
    def apply(resultType: Type)(implicit ctx: Context) =
      unique(new CachedExprType(resultType))
  }

  case class PolyType(paramNames: List[TypeName])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type)
      extends UncachedGroundType with BindingType {
    lazy val paramBounds = paramBoundsExp(this)
    override lazy val resultType = resultTypeExp(this)

    override def signature(implicit ctx: Context) = resultType.signature

    def instantiate(argTypes: List[Type])(implicit ctx: Context): Type =
      new InstPolyMap(this, argTypes) apply resultType

    def instantiateBounds(argTypes: List[Type])(implicit ctx: Context): List[TypeBounds] =
      paramBounds mapConserve (new InstPolyMap(this, argTypes).apply)

    def derivedPolyType(paramNames: List[TypeName], paramBounds: List[TypeBounds], restpe: Type)(implicit ctx: Context) =
      if ((paramNames eq this.paramNames) && (paramBounds eq this.paramBounds) && (restpe eq this.resultType)) this
      else
        PolyType(paramNames)(
          x => paramBounds mapConserve (_.substBounds(this, x)),
          x => restpe.subst(this, x))

    // need to override hashCode and equals to be object identity
    // because paramNames by itself is not discriminatory enough
    override def hashCode = System.identityHashCode(this)
    override def equals(other: Any) = other match {
      case that: PolyType => this eq that
      case _ => false
    }
  }

  object PolyType {
    def fromSymbols(tparams: List[Symbol], resultType: Type)(implicit ctx: Context) = {
      def transform(pt: PolyType, tp: Type) =
        tp.subst(tparams, (0 until tparams.length).toList map (PolyParam(pt, _)))
      apply(tparams map (_.name.asTypeName))(
          pt => tparams map (tparam => transform(pt, tparam.info).bounds),
          pt => transform(pt, resultType))
    }
  }

  abstract class BoundType extends UncachedProxyType {
    type BT <: BindingType
    def binder: BT
    def copy(bt: BT): Type
  }

  case class MethodParam(binder: MethodType, paramNum: Int) extends BoundType with SingletonType {
    type BT = MethodType
    override def underlying(implicit ctx: Context) = binder.paramTypes(paramNum)
    override def hashCode = doHash(System.identityHashCode(binder) + paramNum)
    def copy(bt: BT) = MethodParam(bt, paramNum)
  }

  case class PolyParam(binder: PolyType, paramNum: Int) extends BoundType {
    type BT = PolyType
    override def underlying(implicit ctx: Context) = binder.paramBounds(paramNum).hi
    def copy(bt: BT) = PolyParam(bt, paramNum)
    // no hashCode needed because cycle is broken in PolyType
  }

  case class RefinedThis(binder: RefinedType) extends BoundType with SingletonType {
    type BT = RefinedType
    override def underlying(implicit ctx: Context) = binder.parent
    def copy(bt: BT) = RefinedThis(bt)
    override def hashCode = doHash(System.identityHashCode(binder))
  }

  // ------ ClassInfo, Type Bounds ------------------------------------------------------------

  /** The info of a class during a period.
   *  @param pre          The prefix on which all class attributes need to be rebased.
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

    def typeConstructor(implicit ctx: Context): Type =
      NamedType(prefix, cls.name)

    // cached because baseType needs parents
    private var parentsCache: List[TypeRef] = null

    def rebase(tp: Type)(implicit ctx: Context): Type =
      tp.substThis(cls, prefix)

    override def parents(implicit ctx: Context): List[TypeRef] = {
      if (parentsCache == null)
        parentsCache = classParents.mapConserve(rebase(_).asInstanceOf[TypeRef])
      parentsCache
    }

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

  abstract case class TypeBounds(lo: Type, hi: Type) extends CachedProxyType with TypeType {
    override def underlying(implicit ctx: Context): Type = hi
    def derivedTypeBounds(lo1: Type, hi1: Type)(implicit ctx: Context) =
      if ((lo1 eq lo) && (hi1 eq hi)) this
      else TypeBounds(lo, hi)

    def contains(tp: Type)(implicit ctx: Context) = lo <:< tp && tp <:< hi

    def &(that: TypeBounds)(implicit ctx: Context): TypeBounds =
      TypeBounds(this.lo | that.lo, this.hi & that.hi)
    def |(that: TypeBounds)(implicit ctx: Context): TypeBounds =
      TypeBounds(this.lo & that.lo, this.hi | that.hi)

    def substBounds(from: PolyType, to: PolyType)(implicit ctx: Context) =
      subst(from, to).asInstanceOf[TypeBounds]

    def map(f: Type => Type)(implicit ctx: Context): TypeBounds =
      TypeBounds(f(lo), f(hi))

    override def computeHash = doHash(lo, hi)
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

  case class AnnotatedType(annot: Annotation, tpe: Type) extends UncachedProxyType {
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

  case class ImportType(expr: SharedTree) extends UncachedGroundType

  // Special type objects ------------------------------------------------------------

  case object NoType extends UncachedGroundType {
    def symbol = NoSymbol
    def info = NoType
  }

  /** Cached for efficiency because hashing is faster */
  case object NoPrefix extends CachedGroundType {
    override def computeHash = hashSeed
  }

  abstract class ErrorType extends UncachedGroundType

  object ErrorType extends ErrorType

  case object WildcardType extends UncachedGroundType

  // ----- TypeMaps --------------------------------------------------------------------

  abstract class TypeMap(implicit ctx: Context) extends (Type => Type) { thisMap =>
    def apply(tp: Type): Type

    def applyToBounds(tp: TypeBounds): TypeBounds =
      apply(tp: Type).asInstanceOf[TypeBounds]

    /** Map this function over given type */
    def mapOver(tp: Type): Type = tp match {
      case tp: NamedType =>
        tp.derivedNamedType(this(tp.prefix))

      case _: ThisType
         | _: BoundType => tp

      case tp: RefinedType =>
        tp.derivedRefinedType(this(tp.parent), tp.name, this(tp.info))

      case tp @ PolyType(pnames) =>
        tp.derivedPolyType(
          pnames, tp.paramBounds mapConserve applyToBounds, this(tp.resultType))

      case tp @ MethodType(pnames, ptypes) =>
        tp.derivedMethodType(pnames, ptypes mapConserve this, this(tp.resultType))

      case tp @ ExprType(restpe) =>
        tp.derivedExprType(this(restpe))

      case tp @ SuperType(thistp, supertp) =>
        tp.derivedSuperType(this(thistp), this(supertp))

      case tp @ TypeBounds(lo, hi) =>
        if (lo eq hi) {
          val lo1 = this(lo)
          tp.derivedTypeBounds(lo1, lo1)
        } else {
          tp.derivedTypeBounds(this(lo), this(hi))
        }

      case tp @ AnnotatedType(annot, underlying) =>
        tp.derivedAnnotatedType(mapOver(annot), this(underlying))

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

    def mapOver(tree: Tree): Tree = new TreeMapper(this).apply(tree)


    def andThen(f: Type => Type): TypeMap = new TypeMap {
      def apply(tp: Type) = f(thisMap.apply(tp))
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
    def apply(bounds: TypeBounds): TypeBounds =
      bounds.derivedTypeBounds(apply(bounds.lo), apply(bounds.hi))
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
        this(this(x, tp.parent), tp.info)

      case tp @ PolyType(pnames) =>
        this((x /: tp.paramBounds)(this), tp.resultType)

      case tp @ MethodType(pnames, ptypes) =>
        this((x /: ptypes)(this), tp.resultType)

      case ExprType(restpe) =>
        this(x, restpe)

      case SuperType(thistp, supertp) =>
        this(this(x, thistp), supertp)

      case TypeBounds(lo, hi) =>
        this(this(x, lo), hi)

      case AnnotatedType(annot, underlying) =>
        this(this(x, annot), underlying)

      case _ => x
    }
  }

  class ExistsAccumulator(p: Type => Boolean) extends TypeAccumulator[Boolean] {
    def apply(x: Boolean, tp: Type) = x || p(tp) || foldOver(x, tp)
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
      name.isTypeName && (pre member name).info.isRealTypeBounds
  }

  /** A filter for names of deferred term definitions of a given type */
  object abstractTermNameFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean =
      name.isTermName && (pre member name).hasAltWith(_ is Deferred)
  }

  // ----- Exceptions -------------------------------------------------------------

  class TypeError(msg: String) extends Exception(msg)
  class FatalTypeError(msg: String) extends TypeError(msg)
  class MalformedType(pre: Type, sym: Symbol) extends FatalTypeError(s"malformed type: $pre.$sym")
  class CyclicReference(sym: Symbol) extends FatalTypeError("cyclic reference involving $sym")

  // ----- Misc utilities ---------------------------------------------------------

  /** like map2, but returns list `xs` itself - instead of a copy - if function
   *  `f` maps all elements to themselves.
   */
  def map2Conserve[A <: AnyRef, B](xs: List[A], ys: List[B])(f: (A, B) => A): List[A] =
    if (xs.isEmpty) xs
    else {
      val x1 = f(xs.head, ys.head)
      val xs1 = map2Conserve(xs.tail, ys.tail)(f)
      if ((x1 eq xs.head) && (xs1 eq xs.tail)) xs
      else x1 :: xs1
    }

  /** True if two lists have the same length.  Since calling length on linear sequences
   *  is O(n), it is an inadvisable way to test length equality.
   */
  final def sameLength[T](xs: List[T], ys: List[T]): Boolean = xs match {
    case _ :: xs1 =>
      ys match {
        case _ :: ys1 => sameLength(xs1, ys1)
        case _ => false
      }
    case _ => ys.isEmpty
  }
}
