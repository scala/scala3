package dotty.tools.dotc
package core

import util.HashSet
import Symbols._
import SubTypers._
import Flags._
import Names._
import Scopes._
import Substituters._
import Constants._
import Contexts._
import Annotations._
import Denotations._
import References._
import Periods._
import References.{Reference, RefSet, RefUnion, ErrorRef}
import scala.util.hashing.{MurmurHash3 => hashing}
import collection.mutable

trait Types { self: Context =>

  import Types._

  private val initialUniquesCapacity = 50000

  private val uniques = new util.HashSet[Type]("uniques", initialUniquesCapacity) {
    override def hash(x: Type): Int = x.hash
  }

  private var volatileRecursions: Int = 0
  private val pendingVolatiles = new mutable.HashSet[Type]
}

object Types {

  /** A hash value indicating that the underlying type is not
   *  cached in uniques.
   */
  final val NotCached = 0

  /** An alternative value returned from `hash` if the
   *  computed hashCode would be `NotCached`.
   */
  private final val NotCachedAlt = Int.MinValue

  /** How many recursive calls to isVolatile are performed before
   *  logging starts.
   */
  private final val LogVolatileThreshold = 50

  /** The class of types.
   *  The principal subclasses and sub-objects are as follows:
   *
   *  Type -+- TypeProxy -+- NamedType ----+--- TypeRef
   *        |             |                 \
   *        |             +- SingletonType---+- TermRef
   *        |             |
   *        |             +- SingletonType --+- ThisType
   *        |             |                  +- SuperType
   *        |             |                  +- ConstantType
   *        |             |                  +- MethodParam
   *        |             |                  +- RefinedThis
   *        |             |                  +- TypeBounds
   *        |             |                  +- ExprType
   *        |             |                  +- AnnotatedType
   *        |             +- PolyParam
   *        |             +- AppliedType
   *        |             +- RefinedType
   *        +- AndType
   *        +- OrType
   *        +- MethodType -+- ImplicitMethodType
   *        |              +- JavaMethodType
   *        +- PolyType
   *        +- ClassInfo
   *        |
   *        +- NoType
   *        +- ErrorType
   *        +- WildcardType
   */
  abstract class Type extends DotClass {

    def hash = NotCached

    /** The type symbol associated with the type */
    def typeSymbol(implicit ctx: Context): Symbol = NoSymbol

    /** The term symbol associated with the type */
    def termSymbol(implicit ctx: Context): Symbol = NoSymbol

    /** Does this type denote a stable reference (i.e. singleton type)? */
    def isStable(implicit ctx: Context): Boolean = false

    /** A type T is a legal prefix in a type selection T#A if
     *  T is stable or T contains no uninstantiated type variables.
     */
    def isLegalPrefix(implicit ctx: Context): Boolean =
      isStable || abstractTypeNames(this).isEmpty

    /** The set of names that denote an abstract type member of this type
     *  which is also an abstract type member of `pre`
     */
    def abstractTypeNames(pre: Type)(implicit ctx: Context): Set[Name] =
      memberNames(pre, abstractTypeNameFilter)

    /** The set of names that denote an abstract term member of this type
     *  which is also an abstract term member of `pre`
     */
    def abstractTermNames(pre: Type)(implicit ctx: Context): Set[Name] =
      memberNames(pre, abstractTermNameFilter)

    /** The set of names that denote an abstract member of this type
     *  which is also an abstract member of `pre`
     */
    def abstractMemberNames(pre: Type)(implicit ctx: Context): Set[Name] =
      abstractTypeNames(pre) | abstractTermNames(pre)

    /** The set of names of members of this type that pass the given name filter
     *  when seen as members of `pre`. More precisely, these are all
     *  of members `name` such that `keepOnly(pre, name)` is `true`.
     */
    def memberNames(pre: Type, keepOnly: NameFilter)(implicit ctx: Context): Set[Name] =
      Set()

    /** Is this type a TypeBounds instance, with lower and upper bounds
     *  that are not identical?
     */
    def isRealTypeBounds: Boolean = false

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
    def isVolatile(implicit ctx: Context): Boolean = {
      def isAbstractIntersection(tp: Type): Boolean = tp match {
        case tp: TypeRef => tp.isAbstractType
        case AndType(l, r) => isAbstractIntersection(l) | isAbstractIntersection(l)
        case OrType(l, r) => isAbstractIntersection(l) & isAbstractIntersection(r)
        case _ => false
      }
      def test = {
        this match {
          case ThisType(_) =>
            false
          case RefinedType(p, names) =>
            p.isVolatile ||
              isAbstractIntersection(p) &&
              (names exists (abstractMemberNames(this) contains))
          case tp: TypeProxy =>
            tp.underlying.isVolatile
          case AndType(l, r) =>
            l.isVolatile || r.isVolatile ||
              isAbstractIntersection(l) && r.abstractMemberNames(this).nonEmpty
          case OrType(l, r) =>
            l.isVolatile && r.isVolatile
          case _ =>
            false
        }
      }
      // need to be careful not to fall into an infinite recursion here
      // because volatile checking is done before all cycles are detected.
      // the case to avoid is an abstract type directly or
      // indirectly upper-bounded by itself. See #2918
      import ctx.root.{volatileRecursions, pendingVolatiles}
      try {
        volatileRecursions += 1
        if (volatileRecursions < LogVolatileThreshold)
          test
        else if (pendingVolatiles(this))
          false // we can return false here, because a cycle will be detected
                // here afterwards and an error will result anyway.
        else
          try {
            pendingVolatiles += this
            test
          } finally {
            pendingVolatiles -= this
          }
      } finally {
        volatileRecursions -= 1
      }
    }

    /** Is this type guaranteed not to have `null` as a value? */
    def isNotNull: Boolean = false

    /** Is this type produced as a repair for an error? */
    def isError(implicit ctx: Context): Boolean = (typeSymbol hasFlag Error) || (termSymbol hasFlag Error)

    /** Is some part of this type produced as a repair for an error? */
    def isErroneous(implicit ctx: Context): Boolean = exists(_.isError)

    /** Returns true if there is a part of this type that satisfies predicate `p`.
     */
    def exists(p: Type => Boolean): Boolean =
      new ExistsAccumulator(p)(false, this)

    /** Substitute all types that refer in their symbol attribute to
     *  one of the symbols in `from` by the corresponding types in `to`
     */
    def subst(from: List[Symbol], to: List[Type])(implicit ctx: Context): Type =
      if (from.isEmpty) this
      else {
        val from1 = from.tail
        if (from1.isEmpty) new SubstOps(this).subst1(from.head, to.head, null)
        else {
          val from2 = from1.tail
          if (from2.isEmpty) new SubstOps(this).subst2(from.head, to.head, from.tail.head, to.tail.head, null)
          else new SubstOps(this).subst(from, to, null)
        }
      }

    /** Substitute all types of the form `PolyParam(from, N)` by
     *  `PolyParam(to, N)`.
     */
    def subst(from: PolyType, to: PolyType)(implicit ctx: Context): Type =
      new SubstOps(this).subst(from, to, null)

    /** Substitute all types of the form `MethodParam(from, N)` by
     *  `MethodParam(to, N)`.
     */
    def subst(from: MethodType, to: MethodType)(implicit ctx: Context): Type =
      if (from.isDependent) new SubstOps(this).subst(from, to, null)
      else this

    /** Substitute all references of the form `This(clazz)` by `tp` */
    def substThis(clazz: ClassSymbol, tp: Type)(implicit ctx: Context): Type =
      new SubstOps(this).substThis(clazz, tp, null)

    /** Substitute all references of the form `RefinedThis(from)` by `tp` */
    def substThis(from: RefinedType, tp: Type)(implicit ctx: Context): Type =
      new SubstOps(this).substThis(from, tp, null)

    /** For a ClassInfo type, its parents,
     *  For an AndType, its operands,
     *  For an applied type, the instantiated parents of its base type.
     *  Inherited by all type proxies. Empty for all other types.
     */
    def parents(implicit ctx: Context): List[Type] = List()

    /** The normalized prefix of this type is:
     *  For an alias type, the normalized prefix of its alias
     *  For all other named type and class infos: the prefix.
     *  Inherited by all other type proxies.
     *  `NoType` for all other types.
     */
    def normalizedPrefix(implicit ctx: Context): Type = NoType

    /** This type seen as a TypeBounds */
    def bounds(implicit ctx: Context): TypeBounds = TypeBounds(this, this)

    /** The scope of all declarations of this type.
     *  Defined by ClassInfo, inherited by type proxies.
     *  Empty scope for all other types.
     */
    def decls(implicit ctx: Context): Scope = EmptyScope

    /** The declaration of this type with given name */
    def decl(name: Name)(implicit ctx: Context): Reference =
      decls.refsNamed(name).toRef

    /** The member of this type with given name  */
    def member(name: Name)(implicit ctx: Context): Reference =
      findMember(name, this, Flags.Empty)

    /** The non-private member of this type with given name */
    def nonPrivateMember(name: Name)(implicit ctx: Context): Reference =
      findMember(name, this, Flags.Private)

    /** Find member of this type with given name and
     *  produce a reference that contains the type of the member
     *  as seen from given prefix `pre`. Exclude all members with one
     *  of the flags in `excluded` from consideration.
     */
    def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Reference =
      unsupported("findMember")

    /** Is this type a subtype of that type? */
    def <:< (that: Type)(implicit ctx: Context): Boolean =
      ctx.subTyper.isSubType(this, that)

    /** Is this type the same as that type?
     *  This is the case iff `this <:< that` and `that <:< this`.
     */
    def =:= (that: Type)(implicit ctx: Context): Boolean =
      ctx.subTyper.isSameType(this, that)

    /** Widen from singleton type to its underlying non-singleton
     *  base type by applying one or more `underlying` dereferences,
     *  identity for all other types. Example:
     *
     *  class Outer { class C ; val x: C }
     *  val o: Outer
     *  <o.x.type>.widen = o.C
     */
    def widen(implicit ctx: Context): Type = this

    /** Widen from constant type to its underlying non-constant
     *  base type.
     */
    def deconst: Type = this

    //def resultType: Type = ???

    /** The base classes of this type as determined by ClassDenotation.
     *  Inherited by all type proxies.
     *  `Nil` for all other types.
     */
    def baseClasses(implicit ctx: Context): List[ClassSymbol] = Nil


    def asSeenFrom(pre: Type, clazz: Symbol)(implicit ctx: Context): Type =
      if (clazz.isStaticMono || ctx.erasedTypes && clazz != defn.ArrayClass ) this
      else asSeenFrom(pre, clazz, null)

    def asSeenFrom(pre: Type, clazz: Symbol, theMap: AsSeenFromMap)(implicit ctx: Context): Type = {

      def skipPrefixOf(pre: Type, clazz: Symbol) =
        (pre eq NoType) || (pre eq NoPrefix) || clazz.isPackageClass

      def toPrefix(pre: Type, clazz: Symbol, thisclazz: ClassSymbol): Type =
        if (skipPrefixOf(pre, clazz))
          this
        else if ((thisclazz isNonBottomSubClass clazz) &&
          (pre.widen.typeSymbol isNonBottomSubClass thisclazz))
          pre match {
            case SuperType(thistp, _) => thistp
            case _ => pre
          }
        else
          toPrefix(pre.baseType(clazz).normalizedPrefix, clazz.owner, thisclazz)

      def toInstance(pre: Type, clazz: Symbol, tparam: Symbol): Type = {
        if (skipPrefixOf(pre, clazz)) this
        else {
          val tparamOwner = tparam.owner

          def throwError =
            if (tparamOwner.info.parents exists (_.isErroneous))
              ErrorType // don't be overzealous with throwing exceptions, see #2641
            else
              throw new Error(
                s"something is wrong (wrong class file?): tp ${tparam.locationString} cannot be instantiated from ${pre.widen}")

          def prefixMatches = pre.typeSymbol isNonBottomSubClass tparamOwner

          val basePre = pre.baseType(clazz)

          def instParamFrom(typeInst: Type): Type = typeInst match {
            case ConstantType(_) =>
              // have to deconst because it may be a Class[T].
              instParamFrom(typeInst.deconst)
            case AppliedType(tycon, baseArgs) =>
              instParam(tycon.typeParams, baseArgs)
            case _ =>
              throwError
          }

          def instParam(ps: List[Symbol], as: List[Type]): Type =
            if (ps.isEmpty || as.isEmpty) throwError
            else if (tparam eq ps.head) as.head
            else throwError

          if (tparamOwner == clazz && prefixMatches) instParamFrom(basePre)
          else toInstance(basePre.normalizedPrefix, clazz.owner, tparam)
        }
      }

      this match {
        case tp: NamedType =>
          val sym = tp.symbol
          if (tp.symbol.isTypeParameter) toInstance(pre, clazz, sym)
          else if (sym.isStatic) this
          else tp.derivedNamedType(tp.prefix.asSeenFrom(pre, clazz, theMap), tp.name)
        case ThisType(thisclazz) =>
          toPrefix(pre, clazz, thisclazz)
        case _ =>
          val asSeenFromMap = if (theMap != null) theMap else new AsSeenFromMap(pre, clazz)
          this match {
            case tp: AppliedType =>
              tp.derivedAppliedType(
                asSeenFromMap(tp.tycon), tp.targs mapConserve asSeenFromMap)
            case _ =>
              asSeenFromMap mapOver this
          }
      }
    }

    def signature: Signature = NullSignature
    def subSignature: Signature = List()

    def baseType(base: Symbol)(implicit ctx: Context): Type = base.deref match {
      case classd: ClassDenotation => classd.baseTypeOf(this)
      case _ => NoType
    }

    /** The type parameters of this type are:
     *  For a ClassInfo type, the type parameters of its denotation.
     *  For an applied type, the type parameters of its constructor
     *  that have not been instantiated yet.
     *  Inherited by type proxies.
     *  Empty list for all other types.
     */
    def typeParams(implicit ctx: Context): List[TypeSymbol] = Nil

    /** The type arguments of this type are:
     *  For an Applied type, its type arguments.
     *  Inherited by type proxies.
     *  Empty list for all other types.
     */
    def typeArgs(implicit ctx: Context): List[Type] = Nil

    def isWrong: Boolean = !exists // !!! needed?
    def exists: Boolean = true

    def & (that: Type)(implicit ctx: Context): Type =
      if (this eq that) this
      else if (this.isWrong) that
      else if (that.isWrong) this
      else that match {
        case OrType(that1, that2) =>
          this & that1 | this & that2
        case _ =>
          this match {
            case OrType(this1, this2) =>
              this1 & that | this2 & that
            case _ =>
              val t1 = mergeIfSub(this, that)
              if (t1.exists) t1
              else {
                val t2 = mergeIfSub(that, this)
                if (t2.exists) t2
                else AndType(this, that)
              }
          }
      }

    def | (that: Type)(implicit ctx: Context): Type =
      if (this eq that) this
      else if (this.isWrong) this
      else if (that.isWrong) that
      else {
        val t1 = mergeIfSuper(this, that)
        if (t1.exists) t1
        else {
          val t2 = mergeIfSuper(that, this)
          if (t2.exists) t2
          else OrType(this, that)
        }
      }

    /** Merge `t1` into `t2` if t1 is a subtype of some part of t2.
     */
    private def mergeIfSub(t1: Type, t2: Type)(implicit ctx: Context): Type =
      if (t1 <:< t2)
        if (t2 <:< t1) t2 else t1
      else t2 match {
        case t2 @ AndType(t21, t22) =>
          val lower1 = mergeIfSub(t1, t21)
          if (lower1 eq t21) t2
          else if (lower1.exists) lower1 & t22
          else {
            val lower2 = mergeIfSub(t1, t22)
            if (lower2 eq t22) t2
            else if (lower2.exists) t21 & lower2
            else NoType
          }
        case _ =>
          NoType
      }

   /** Merge `t1` into `t2` if t1 is a supertype of some part of t2.
    */
    private def mergeIfSuper(t1: Type, t2: Type)(implicit ctx: Context): Type =
      if (t2 <:< t1)
        if (t1 <:< t2) t2 else t1
      else t2 match {
        case t2 @ OrType(t21, t22) =>
          val higher1 = mergeIfSuper(t1, t21)
          if (higher1 eq t21) t2
          else if (higher1.exists) higher1 | t22
          else {
            val higher2 = mergeIfSuper(t1, t22)
            if (higher2 eq t22) t2
            else if (higher2.exists) t21 | higher2
            else NoType
          }
        case _ =>
          NoType
      }

    // hashing

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

    protected def doHash(tp1: Type, tp2: Type): Int = {
      val elemHash = tp1.hash
      if (elemHash == NotCached) return NotCached
      finishHash(hashing.mix(hashSeed, elemHash), 1, tp2)
    }

    protected def doHash(x1: Any, tp2: Type): Int =
      finishHash(hashing.mix(hashSeed, x1.hashCode), 1, tp2)

    protected def doHash(tp1: Type, tps2: List[Type]): Int =
      finishHash(hashSeed, 0, tp1, tps2)

    protected def doHash(x1: Any, tp2: Type, tps3: List[Type]): Int =
      finishHash(hashing.mix(hashSeed, x1.hashCode), 1, tp2, tps3)
  } // end Type

  abstract class UniqueType extends Type {
    final override val hash = computeHash
    override def hashCode = hash
    def computeHash: Int
  }

  def unique[T <: Type](tp: T)(implicit ctx: Context): T = {
    if (tp.hash == NotCached) tp
    else ctx.root.uniques.findEntryOrUpdate(tp).asInstanceOf[T]
  }

  trait TypeProxy extends Type {
    def underlying(implicit ctx: Context): Type
    override def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Reference =
      underlying.findMember(name, pre, excluded)
    override def parents(implicit ctx: Context) = underlying.parents
    override def decls(implicit ctx: Context) = underlying.decls
    override def baseClasses(implicit ctx: Context) = underlying.baseClasses
    override def memberNames(pre: Type, keepOnly: NameFilter)(implicit ctx: Context) =
      underlying.memberNames(pre, keepOnly)
    override def isVolatile(implicit ctx: Context): Boolean = underlying.isVolatile
    override def normalizedPrefix(implicit ctx: Context) = underlying.normalizedPrefix
    override def typeParams(implicit ctx: Context) = underlying.typeParams
    override def typeArgs(implicit ctx: Context) = underlying.typeArgs
  }

  trait TransformingProxy extends TypeProxy {
    // needed?
  }

  trait SubType extends UniqueType with TypeProxy {

  }

  trait SingletonType extends SubType {
    override def isStable(implicit ctx: Context) = true
    override def widen(implicit ctx: Context): Type = underlying.widen
  }

// --- NamedTypes ------------------------------------------------------------------

  /** A NamedType of the form Prefix # name
   */
  abstract class NamedType extends UniqueType with TypeProxy {

    val prefix: Type
    val name: Name

    private[this] var referencedVar: Reference = null
    protected[this] var validPeriods = Nowhere

    private def checkPrefix(sym: Symbol) =
      sym.isAbstractType || sym.isClass

    def referenced(implicit ctx: Context): Reference = {
      if (!containsPeriod(validPeriods, ctx.period)) {
        referencedVar = prefix.member(name)
        validPeriods = ctx.stableInterval
        if (checkPrefix(referencedVar.symbol) && !prefix.isLegalPrefix)
          throw new MalformedType(prefix, referencedVar.symbol)
      }
      referencedVar
    }

    def isType = name.isTypeName
    def isTerm = name.isTermName

    def symbol(implicit ctx: Context): Symbol = referenced.symbol
    def info(implicit ctx: Context): Type = referenced.info

    def underlying(implicit ctx: Context): Type = info

    def isAbstractType(implicit ctx: Context) = info.isRealTypeBounds

    override def normalizedPrefix(implicit ctx: Context) =
      if (isAbstractType) info.normalizedPrefix else prefix

    def derivedNamedType(prefix: Type, name: Name)(implicit ctx: Context): Type =
      if (prefix eq this.prefix) this
      else NamedType(prefix, name)

    override def computeHash = doHash(name, prefix)
  }

  abstract case class TermRef(override val prefix: Type, name: TermName) extends NamedType with SingletonType {
    override def termSymbol(implicit ctx: Context): Symbol = symbol
    override def isStable(implicit ctx: Context) = prefix.isStable && termSymbol.isStable
  }

  abstract case class TypeRef(override val prefix: Type, name: TypeName) extends NamedType {
    override def typeSymbol(implicit ctx: Context): Symbol = symbol
  }

  trait NamedNoPrefix extends NamedType {
    protected val fixedSym: Symbol
    override def symbol(implicit ctx: Context): Symbol = fixedSym
    override def info(implicit ctx: Context): Type = fixedSym.info
    override def referenced(implicit ctx: Context): Reference = new UniqueSymRef(fixedSym, info)
  }

  final class TermRefNoPrefix(val fixedSym: TermSymbol)(implicit ctx: Context)
        extends TermRef(NoPrefix, fixedSym.name) with NamedNoPrefix {
    validPeriods = allPeriods(ctx.runId)
  }

  final class TermRefWithSignature(prefix: Type, name: TermName, override val signature: Signature) extends TermRef(prefix, name) {
    override def computeHash = doHash((name, signature), prefix)
    override def referenced(implicit ctx: Context): Reference =
      super.referenced.atSignature(signature)
  }

  final class TypeRefNoPrefix(val fixedSym: TypeSymbol)(implicit ctx: Context)
        extends TypeRef(NoPrefix, fixedSym.name) with NamedNoPrefix {
    validPeriods = allPeriods(ctx.runId)
  }

  final class UniqueTermRef(prefix: Type, name: TermName) extends TermRef(prefix, name)
  final class UniqueTypeRef(prefix: Type, name: TypeName) extends TypeRef(prefix, name)

  object NamedType {
    def apply(prefix: Type, name: Name)(implicit ctx: Context) =
      if (name.isTermName) TermRef(prefix, name.asTermName)
      else TypeRef(prefix, name.asTypeName)
  }

  object TermRef {
    def apply(prefix: Type, name: TermName)(implicit ctx: Context) =
      unique(new UniqueTermRef(prefix, name))
    def apply(sym: TermSymbol)(implicit ctx: Context) =
      unique(new TermRefNoPrefix(sym))
    def apply(prefix: Type, name: TermName, signature: Signature)(implicit ctx: Context) =
      unique(new TermRefWithSignature(prefix, name, signature))
  }

  object TypeRef {
    def apply(prefix: Type, name: TypeName)(implicit ctx: Context) =
      unique(new UniqueTypeRef(prefix, name))
    def apply(sym: TypeSymbol)(implicit ctx: Context) =
      unique(new TypeRefNoPrefix(sym))
  }

// --- Other SingletonTypes: ThisType/SuperType/ConstantType ---------------------------

  abstract case class ThisType(clazz: ClassSymbol) extends SingletonType {
    def underlying(implicit ctx: Context) = clazz.typeOfThis
    override def isVolatile(implicit ctx: Context): Boolean = false
    override def computeHash = doHash(clazz)
  }

  final class UniqueThisType(clazz: ClassSymbol) extends ThisType(clazz)

  object ThisType {
    def apply(clazz: ClassSymbol)(implicit ctx: Context) =
      unique(new UniqueThisType(clazz))
  }

  abstract case class SuperType(thistpe: Type, supertpe: Type) extends SingletonType {
    def underlying(implicit ctx: Context) = supertpe
    def derivedSuperType(thistp: Type, supertp: Type)(implicit ctx: Context) =
      if ((thistp eq thistpe) && (supertp eq supertpe)) this
      else SuperType(thistp, supertp)
    override def computeHash = doHash(thistpe, supertpe)
  }

  final class UniqueSuperType(thistpe: Type, supertpe: Type) extends SuperType(thistpe, supertpe)

  object SuperType {
    def apply(thistpe: Type, supertpe: Type)(implicit ctx: Context) =
      unique(new UniqueSuperType(thistpe, supertpe))
  }

  abstract case class ConstantType(value: Constant) extends SingletonType {
    def underlying(implicit ctx: Context) = value.tpe
    override def deconst: Type = value.tpe
    override def computeHash = doHash(value)
  }

  final class UniqueConstantType(value: Constant) extends ConstantType(value)

  object ConstantType {
    def apply(value: Constant)(implicit ctx: Context) =
      unique(new UniqueConstantType(value))
  }

  // --- AppliedType -----------------------------------------------------------------

  abstract case class AppliedType(tycon: Type, targs: List[Type]) extends UniqueType with TypeProxy {

    def underlying(implicit ctx: Context) = tycon

    def derivedAppliedType(tycon: Type, targs: List[Type])(implicit ctx: Context): Type =
      if ((tycon eq this.tycon) && (targs eq this.targs)) this
      else AppliedType(tycon, targs)

    override def computeHash = doHash(tycon, targs)

    override def typeParams(implicit ctx: Context): List[TypeSymbol] =
      tycon.typeParams drop targs.length

    override def typeArgs(implicit ctx: Context): List[Type] = targs

    override def parents(implicit ctx: Context) =
      tycon.parents.mapConserve(_.subst(tycon.typeParams, targs))

  }
  final class UniqueAppliedType(tycon: Type, targs: List[Type]) extends AppliedType(tycon, targs)

  object AppliedType {
    def apply(tycon: Type, targs: List[Type])(implicit ctx: Context) =
      unique(new UniqueAppliedType(tycon, targs))
    def make(tycon: Type, targs: List[Type])(implicit ctx: Context) =
      if (targs.isEmpty) tycon else apply(tycon, targs)
  }

// --- Refined Type ---------------------------------------------------------

  case class RefinedType(parent: Type, names: List[Name])(infosExpr: RefinedType => List[Type]) extends UniqueType with TypeProxy {

    def underlying(implicit ctx: Context) = parent

    lazy val infos = infosExpr(this)

    def derivedRefinedType(parent1: Type, names1: List[Name], infos1: List[Type])(implicit ctx: Context): RefinedType =
      if ((parent1 eq parent) && (names1 eq names) && (infos1 eq infos)) this
      else
        RefinedType(parent1, names1) { rt =>
          val thistp = RefinedThis(rt)
          infos1 map (_.substThis(this, thistp))
        }

    def findDecl(name: Name, pre: Type)(implicit ctx: Context): Reference = {
      var ns = names
      var is = infos
      var ref: Reference = NoRef
      while (ns.nonEmpty && (ref eq NoRef)) {
        if (ns.head == name)
          ref = new JointSymRef(NoSymbol, is.head.substThis(this, pre))
        ns = ns.tail
        is = is.tail
      }
      ref
    }

    override def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Reference =
      parent.findMember(name, pre, excluded | Flags.Private) &
      findDecl(name, pre)

    override def memberNames(pre: Type, keepOnly: NameFilter)(implicit ctx: Context): Set[Name] =
      parent.memberNames(pre, keepOnly) ++ (names filter (keepOnly(pre, _))).toSet

    def computeHash = doHash(names, parent, infos)
  }


// --- AndType/OrType ---------------------------------------------------------------

  abstract case class AndType(tp1: Type, tp2: Type) extends UniqueType {

    type This <: AndType

    def derivedAndType(t1: Type, t2: Type)(implicit ctx: Context) =
      if ((t1 eq tp1) && (t2 eq tp2)) this
      else AndType(t1, t2)

    override def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Reference =
      tp1.findMember(name, pre, excluded) & tp2.findMember(name, pre, excluded)

    override def memberNames(pre: Type, keepOnly: NameFilter)(implicit ctx: Context): Set[Name] =
      tp1.memberNames(pre, keepOnly) | tp2.memberNames(pre, keepOnly)

    override def parents(implicit ctx: Context): List[Type] = {
      def components(tp: Type): List[Type] = tp match {
        case AndType(tp1, tp2) => components(tp1) ++ components(tp2)
        case _ => List(tp)
      }
      components(this)
    }

    override def computeHash = doHash(tp1, tp2)
  }

  final class UniqueAndType(tp1: Type, tp2: Type) extends AndType(tp1, tp2)

  object AndType {
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context) =
      unique(new UniqueAndType(tp1, tp2))
  }

  abstract case class OrType(tp1: Type, tp2: Type) extends UniqueType {
    def derivedOrType(t1: Type, t2: Type)(implicit ctx: Context) =
      if ((t1 eq tp1) && (t2 eq tp2)) this
      else OrType(t1, t2)

    override def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Reference = {
      (tp1.findMember(name, pre, excluded) | tp2.findMember(name, pre, excluded))(pre)
    }

    override def memberNames(pre: Type, keepOnly: NameFilter)(implicit ctx: Context): Set[Name] =
      tp1.memberNames(pre, keepOnly) & tp2.memberNames(pre, keepOnly)

    override def computeHash = doHash(tp1, tp2)
  }

  final class UniqueOrType(tp1: Type, tp2: Type) extends OrType(tp1, tp2)

  object OrType {
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context) =
      unique(new UniqueOrType(tp1, tp2))
  }

// ----- Method types: MethodType/ExprType/PolyType/MethodParam/PolyParam ---------------

  abstract case class MethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type) extends UniqueType {
    lazy val resultType = resultTypeExp(this)
    def isJava = false
    def isImplicit = false
    lazy val isDependent = resultType exists {
      case MethodParam(mt, _) => mt eq this
      case _ => false
    }
    def paramSig(tp: Type): TypeName = ???
    override lazy val signature: Signature =
      (paramTypes map paramSig) ++ resultType.subSignature

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

  final class UniqueMethodType(paramNames: List[TermName], paramTypes: List[Type])
      (resultTypeExp: MethodType => Type)
      extends MethodType(paramNames, paramTypes)(resultTypeExp)
  final class JavaMethodType(paramNames: List[TermName], paramTypes: List[Type])
      (resultTypeExp: MethodType => Type)
      extends MethodType(paramNames, paramTypes)(resultTypeExp) {
    override def isJava = true
  }
  final class ImplicitMethodType(paramNames: List[TermName], paramTypes: List[Type])
      (resultTypeExp: MethodType => Type)
      extends MethodType(paramNames, paramTypes)(resultTypeExp) {
    override def isImplicit = true
  }

  object MethodType {
    def apply(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context) =
      unique(new UniqueMethodType(paramNames, paramTypes)(resultTypeExp))
  }
  def JavaMethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context) =
    unique(new JavaMethodType(paramNames, paramTypes)(resultTypeExp))
  def ImplicitMethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type)(implicit ctx: Context) =
    unique(new ImplicitMethodType(paramNames, paramTypes)(resultTypeExp))

  abstract case class ExprType(resultType: Type) extends UniqueType with TypeProxy {
    def underlying(implicit ctx: Context): Type = resultType
    def derivedExprType(rt: Type)(implicit ctx: Context) =
      if (rt eq resultType) this else ExprType(rt)
    override def computeHash = doHash(resultType)
  }

  final class UniqueExprType(resultType: Type) extends ExprType(resultType)

  object ExprType {
    def apply(resultType: Type)(implicit ctx: Context) =
      unique(new UniqueExprType(resultType))
  }

  case class PolyType(paramNames: List[TypeName])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type) extends Type {
    lazy val paramBounds = paramBoundsExp(this)
    lazy val resultType = resultTypeExp(this)

    def instantiate(argTypes: List[Type])(implicit ctx: Context): Type =
      new InstPolyMap(this, argTypes) apply resultType

    override def signature: Signature = resultType.subSignature

    def derivedPolyType(paramNames: List[TypeName], paramBounds: List[TypeBounds], restpe: Type)(implicit ctx: Context) =
      if ((paramNames eq this.paramNames) && (paramBounds eq this.paramBounds) && (restpe eq this.resultType)) this
      else
        PolyType(paramNames)(
          x => paramBounds mapConserve (_.substBounds(this, x)),
          x => restpe.subst(this, x))

    override def hashCode = System.identityHashCode(this)
    override def equals(other: Any) = other match {
      case that: PolyType => this eq that
      case _ => false
    }
  }

  case class MethodParam(mt: MethodType, paramNum: Int) extends SingletonType {
    def underlying(implicit ctx: Context) = mt.paramTypes(paramNum)
    override def computeHash = NotCached
  }

  case class RefinedThis(rt: RefinedType) extends SingletonType {
    def underlying(implicit ctx: Context) = rt.parent
    override def computeHash = NotCached
  }

  case class PolyParam(pt: PolyType, paramNum: Int) extends TypeProxy {
    def underlying(implicit ctx: Context) = pt.paramBounds(paramNum).hi
  }

// ------ ClassInfo, Type Bounds ------------------------------------------------------------

  abstract case class ClassInfo(prefix: Type, classd: ClassDenotation) extends UniqueType {
    override def typeSymbol(implicit ctx: Context) = classd.clazz

    def typeTemplate(implicit ctx: Context): Type =
      classd.typeTemplate asSeenFrom (prefix, classd.clazz)

    def typeConstructor(implicit ctx: Context): Type =
      NamedType(prefix, classd.clazz.name)

    override def normalizedPrefix(implicit ctx: Context) = prefix

    override def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Reference =
      findMemberAmong(classd.memberRefsNamed(name), pre, classd.clazz, excluded)

    private def findMemberAmong(candidates: RefSet, pre: Type, owner: ClassSymbol, excluded: FlagSet)
        (implicit ctx: Context): Reference = {
      val resultSyms = candidates
        .filterAccessibleFrom(pre)
        .filterExcluded(excluded)
        .asSeenFrom(pre, owner)
      if (resultSyms.exists) resultSyms.toRef
      else ErrorRef // todo: refine
    }

    override def baseClasses(implicit ctx: Context): List[ClassSymbol] =
      classd.baseClasses

    override def memberNames(pre: Type, keepOnly: NameFilter)(implicit ctx: Context): Set[Name] =
      classd.memberNames(keepOnly) filter (keepOnly(pre, _))

    private var parentsCache: List[Type] = null
    // !!! caching needed here? If yes, cache AppliedType as well?

    override def decls(implicit ctx: Context) = classd.decls

    override def parents(implicit ctx: Context) = {
      if (parentsCache == null)
        parentsCache = classd.parents.mapConserve(_.substThis(classd.clazz, prefix))
      parentsCache
    }

    override def typeParams(implicit ctx: Context) = classd.typeParams

    override def computeHash = doHash(classd.clazz, prefix)
  }

  final class UniqueClassInfo(prefix: Type, classd: ClassDenotation) extends ClassInfo(prefix, classd)

  object ClassInfo {
    def apply(prefix: Type, classd: ClassDenotation)(implicit ctx: Context) =
      unique(new UniqueClassInfo(prefix, classd))
  }

  abstract case class TypeBounds(lo: Type, hi: Type) extends UniqueType with TypeProxy {
    def underlying(implicit ctx: Context): Type = hi
    def derivedTypeBounds(lo1: Type, hi1: Type)(implicit ctx: Context) =
      if ((lo1 eq lo) && (hi1 eq hi)) this
      else TypeBounds(lo, hi)

    override def isRealTypeBounds = lo ne hi
    override def bounds(implicit ctx: Context): TypeBounds = this

    def & (that: TypeBounds)(implicit ctx: Context): TypeBounds =
      TypeBounds(this.lo | that.lo, this.hi & that.hi)
    def | (that: TypeBounds)(implicit ctx: Context): TypeBounds =
      TypeBounds(this.lo & that.lo, this.hi | that.hi)

    def substBounds(from: PolyType, to: PolyType)(implicit ctx: Context) =
      subst(from, to).asInstanceOf[TypeBounds]

    def map(f: Type => Type)(implicit ctx: Context): TypeBounds =
      TypeBounds(f(lo), f(hi))
    override def computeHash = doHash(lo, hi)
  }

  final class UniqueTypeBounds(lo: Type, hi: Type) extends TypeBounds(lo, hi)

  object TypeBounds {
    def apply(lo: Type, hi: Type)(implicit ctx: Context) =
      unique(new UniqueTypeBounds(lo, hi))
  }

// ----- AnnotatedTypes -----------------------------------------------------------

  case class AnnotatedType(annots: List[AnnotationInfo], tpe: Type) extends TypeProxy {
    def underlying(implicit ctx: Context): Type = tpe
    def derivedAnnotatedType(annots1: List[AnnotationInfo], tpe1: Type) =
      if ((annots1 eq annots) && (tpe1 eq tpe)) this
      else AnnotatedType.make(annots1, tpe1)
  }

  object AnnotatedType {
    def make(annots: List[AnnotationInfo], underlying: Type) =
      if (annots.isEmpty) underlying
      else AnnotatedType(annots, underlying)
  }

// Special type objects ------------------------------------------------------------

  case object NoType extends Type {
    def symbol = NoSymbol
    def info = NoType
  }

  case object NoPrefix extends UniqueType {
    override def computeHash = hashSeed
  }

  abstract class ErrorType extends Type

  object ErrorType extends ErrorType

  case object WildcardType extends Type

// ----- TypeMaps --------------------------------------------------------------------

  abstract class TypeMap(implicit ctx: Context) extends (Type => Type) {
    def apply(tp: Type): Type

    def applyToBounds(tp: TypeBounds): TypeBounds =
      apply(tp: Type).asInstanceOf[TypeBounds]

    /** Map this function over given type */
    def mapOver(tp: Type): Type = tp match {
      case tp: NamedType   =>
        tp.derivedNamedType(this(tp.prefix), tp.name)

      case ThisType(_)
         | MethodParam(_, _)
         | PolyParam(_, _) => tp

      case tp @ AppliedType(tycon, targs) =>
        tp.derivedAppliedType(this(tycon), targs mapConserve this)

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

      case tp @ RefinedType(parent, names) =>
        tp.derivedRefinedType(this(parent), names, tp.infos mapConserve this)

      case tp @ AnnotatedType(annots, underlying) =>
        tp.derivedAnnotatedType(mapOverAnnotations(annots), this(underlying))

      case _ =>
        tp
    }

    def mapOverAnnotations(annots: List[AnnotationInfo]): List[AnnotationInfo] = ???

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

  class InstRefinedMap(rt: RefinedType)(implicit ctx: Context) extends TypeMap {
    def apply(tp: Type) = tp match {
      case RefinedThis(`rt`) => rt.parent
      case _ => mapOver(tp)
    }
  }

  class AsSeenFromMap(pre: Type, clazz: Symbol)(implicit ctx: Context) extends TypeMap {
    def apply(tp: Type) = tp.asSeenFrom(pre, clazz, this)
  }

// todo: prevent unstable prefixes in variables?


// ----- TypeAccumulators ----------------------------------------------------

  abstract class TypeAccumulator[T] extends ((T, Type) => T) {
    def apply(x: T, tp: Type): T

    def apply(x: T, annot: AnnotationInfo): T = ???

    def foldOver(x: T, tp: Type): T = tp match {
      case tp: NamedType =>
        this(x, tp.prefix)

     case ThisType(_)
         | MethodParam(_, _)
         | PolyParam(_, _)
         | ConstantType(_)
         | NoPrefix => x

      case AppliedType(tycon, targs) =>
        (this(x, tycon) /: targs) (this)

      case tp @ PolyType(pnames) =>
        this((x /: tp.paramBounds) (this), tp.resultType)

      case tp @ MethodType(pnames, ptypes) =>
        this((x /: ptypes) (this), tp.resultType)

      case ExprType(restpe) =>
        this(x, restpe)

      case SuperType(thistp, supertp) =>
        this(this(x, thistp), supertp)

      case TypeBounds(lo, hi) =>
        this(this(x, lo), hi)

      case tp @ RefinedType(parent, names) =>
        (this(x, parent) /: tp.infos) (apply)

      case AnnotatedType(annots, underlying) =>
        this((x /: annots) (apply), underlying)

      case _ => x
    }
  }

  class ExistsAccumulator(p: Type => Boolean) extends TypeAccumulator[Boolean] {
    def apply(x: Boolean, tp: Type) = x || p(tp) || foldOver(x, tp)
  }

// ----- Name Filters --------------------------------------------------

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
      name.isTermName && (pre member name).symbol.isDeferred
  }

// ----- Exceptions -------------------------------------------------------------

  class TypeError(msg: String) extends Exception(msg)
  class FatalTypeError(msg: String) extends TypeError(msg)
  class MalformedType(pre: Type, sym: Symbol) extends FatalTypeError(s"malformed type: $pre.$sym")
  class CyclicReference(sym: Symbol) extends FatalTypeError("cyclic reference involving $sym")

// ----- Implicit decorators ---------------------------------------------------

  implicit def substOps(tp: Type): SubstOps = new SubstOps(tp)

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