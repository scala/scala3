package dotty.tools.dotc
package core

import util.HashSet
import Symbols._
import Flags._
import Names._
import Scopes._
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

  private[Types] val uniques = new util.HashSet[Type]("uniques", initialUniquesCapacity) {
    override def hash(x: Type): Int = x.hash
  }

}

object Types {

  /** A hash value indicating that the underlying type is not
   *  cached in unbiques.
   */
  final val NotCached = 0

  /** An alternative value returned from `hash` if the
   *  computed hashCode would be `NotCached`.
   */
  final val NotCachedAlt = Int.MinValue

  abstract class Type extends DotClass {

    def <:< (that: Type): Boolean = ???

    def hash = NotCached

    /** The type symbol associated with the type
     */
    def typeSymbol(implicit ctx: Context): Symbol = NoSymbol

    /** The term symbol associated with the type
     */
    def termSymbol(implicit ctx: Context): Symbol = NoSymbol

    /** Does this type denote a stable reference (i.e. singleton type)? */
    def isStable(implicit ctx: Context): Boolean = false

    /** Is this type dangerous (i.e. it might contain conflicting
     *  type information when empty, so that it can be constructed
     *  so that type unsoundness results.) A dangerous type has an underlying
     *  type of the form T_1 with T_n { decls }, where one of the
     *  T_i (i > 1) is an abstract type.
     */
    def isVolatile: Boolean = false

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

    /** For a class or intersection type, its parents.
     *  For a TypeBounds type, the parents of its hi bound.
     *  inherited by typerefs, singleton types, and refinement types,
     *  The empty list for all other types */
    def parents(implicit ctx: Context): List[Type] = List()

    def bounds(implicit ctx: Context): TypeBounds = TypeBounds(this, this)

    def member(name: Name)(implicit ctx: Context): Reference =
      findMember(name, this, Flags.Empty)

    def decls: Scope = unsupported("decls")

    def decl(name: Name)(implicit ctx: Context): Reference =
      decls.refsNamed(name).toRef

    def nonPrivateMember(name: Name)(implicit ctx: Context): Reference =
      findMember(name, this, Flags.Private)

    def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Reference =
      unsupported("findMember")

    protected def findMemberAmong(candidates: RefSet, pre: Type, owner: ClassSymbol, excluded: FlagSet)(implicit ctx: Context): Reference = {
      val resultSyms = candidates
        .filterAccessibleFrom(pre)
        .filterExcluded(excluded)
        .asSeenFrom(pre, owner)
      if (resultSyms.exists) resultSyms.toRef
      else ErrorRef // todo: refine
    }

    def memberType(sym: Symbol): Type = ???
    def memberInfo(sym: Symbol): Type = ???

    def widen: Type = ???

    def deconst: Type = ???

    def prefix: Type = ???

    def isTrivial: Boolean = ???

    def resultType: Type = ???

    def baseClasses: List[ClassSymbol] = ???

    def typeArgs: List[Type] = ???

    def isCachable: Boolean = false

    def asSeenFrom(pre: Type, clazz: Symbol)(implicit ctx: Context): Type =
      if (this.isTrivial || clazz.isStaticMono) this
      else new AsSeenFromMap(pre, clazz) apply (this)

    def subst(from: List[Symbol], to: List[Type]): Type = ???
    def subst(from: PolyType, to: PolyType): Type = ???
    def subst(from: MethodType, to: MethodType): Type = ???
    def substSym(from: List[Symbol], to: List[Symbol]): Type = ???
    def substThis(clazz: ClassSymbol, tp: Type): Type = ???
    def substThis(from: RefinedType, tp: Type): Type = ???

    def baseType(base: Symbol)(implicit ctx: Context): Type =
      if (base.isClass) base.asClass.deref.baseTypeOf(this)
      else NoType

    def typeParams: List[TypeSymbol] = ???

    def effectiveBounds: TypeBounds = ???
    def isWrong: Boolean = ???
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
  }

  trait SubType extends UniqueType with TypeProxy {

  }

  trait SingletonType extends SubType

// --- NamedTypes ------------------------------------------------------------------

  /** A NamedType of the form Prefix # name
   */
  abstract class NamedType extends UniqueType with TypeProxy {

    val name: Name

    private[this] var referencedVar: Reference = null
    private[this] var validPeriods = Nowhere

    private def needsStablePrefix(sym: Symbol) =
      sym.isAbstractType || sym.isClass && !sym.isJava

    def referenced(implicit ctx: Context): Reference = {
      if (!containsPeriod(validPeriods, ctx.period)) {
        referencedVar = prefix.member(name)
        validPeriods = ctx.stableInterval
        if (needsStablePrefix(referencedVar.symbol) && !prefix.isStable)
          throw new MalformedType(prefix, referencedVar.symbol)
      }
      referencedVar
    }

    def isType = name.isTypeName
    def isTerm = name.isTermName

    def symbol(implicit ctx: Context): Symbol = referenced.symbol
    def info(implicit ctx: Context): Type = referenced.info

    def underlying(implicit ctx: Context): Type = info

    def derivedNamedType(pre: Type, name: Name)(implicit ctx: Context): Type =
      if (pre eq prefix) this
      else NamedType(pre, name)

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
    extends TermRef(NoPrefix, fixedSym.name) with NamedNoPrefix

  final class TypeRefNoPrefix(name: TypeName, val fixedSym: Symbol)
    extends TypeRef(NoPrefix, name) with NamedNoPrefix

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
  }

  object TypeRef {
    def apply(prefix: Type, name: TypeName)(implicit ctx: Context) =
      unique(new UniqueTypeRef(prefix, name))
    def apply(sym: TypeSymbol)(implicit ctx: Context) =
      unique(new TypeRefNoPrefix(sym.name, sym))
  }

// --- Other SingletonTypes: ThisType/SuperType/ConstantType ---------------------------

  abstract case class ThisType(clazz: ClassSymbol) extends SingletonType {
    def underlying(implicit ctx: Context) = clazz.typeOfThis
    override def isStable(implicit ctx: Context) = true
    override def computeHash = doHash(clazz)
  }

  final class UniqueThisType(clazz: ClassSymbol) extends ThisType(clazz)

  object ThisType {
    def apply(clazz: ClassSymbol)(implicit ctx: Context) =
      unique(new UniqueThisType(clazz))
  }

  abstract case class SuperType(thistpe: Type, supertpe: Type) extends SingletonType {
    def underlying(implicit ctx: Context) = supertpe
    override def isStable(implicit ctx: Context) = true
    override def computeHash = doHash(thistpe, supertpe)
    def derivedSuperType(thistp: Type, supertp: Type)(implicit ctx: Context) =
      if ((thistp eq thistpe) && (supertp eq supertpe)) this
      else SuperType(thistp, supertp)
  }

  final class UniqueSuperType(thistpe: Type, supertpe: Type) extends SuperType(thistpe, supertpe)

  object SuperType {
    def apply(thistpe: Type, supertpe: Type)(implicit ctx: Context) =
      unique(new UniqueSuperType(thistpe, supertpe))
  }

  abstract case class ConstantType(value: Constant) extends SingletonType {
    def underlying(implicit ctx: Context) = value.tpe
    override def computeHash = doHash(value)
  }

  final class UniqueConstantType(value: Constant) extends ConstantType(value)

  object ConstantType {
    def apply(value: Constant)(implicit ctx: Context) =
      unique(new UniqueConstantType(value))
  }

  // --- AppliedType -----------------------------------------------------------------

  abstract case class AppliedType(tycon: Type, override val typeArgs: List[Type]) extends UniqueType {
    assert(tycon.typeParams.length == typeArgs.length)

   def derivedAppliedType(tc: Type, args: List[Type])(implicit ctx: Context): Type =
      if ((tc eq tycon) && (args eq typeArgs)) this
      else AppliedType(tc, args)

    override def computeHash = doHash(tycon, typeArgs)
  }

  final class UniqueAppliedType(tycon: Type, typeArgs: List[Type]) extends AppliedType(tycon, typeArgs)

  object AppliedType {
    def apply(tycon: Type, typeArgs: List[Type])(implicit ctx: Context) =
      unique(new UniqueAppliedType(tycon, typeArgs))
  }

// --- Refined Type ---------------------------------------------------------

  case class RefinedType(parent: Type, names: List[Name])(infosExpr: RefinedType => List[Type]) extends UniqueType with TypeProxy {

    def underlying(implicit ctx: Context) = parent

    lazy val infos = infosExpr(this)

    def derivedRefinedType(parent1: Type, names1: List[Name], infos1: List[Type]): RefinedType =
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

    def computeHash = doHash(names, parent, infos)
  }

// --- ClassInfo Type ---------------------------------------------------------

  case class ClassInfoType(clazz: ClassSymbol) extends UniqueType {

    def cinfo(implicit ctx: Context): ClassDenotation = clazz.deref

    override def parents(implicit ctx: Context) = cinfo.parents
    def decls(implicit ctx: Context) = cinfo.decls

    override def typeSymbol(implicit ctx: Context) = clazz

    override def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Reference =
      findMemberAmong(cinfo.memberRefsNamed(name), pre, clazz, excluded)

    def computeHash = clazz.hashCode
  }

// --- AndType/OrType ---------------------------------------------------------------

  abstract case class AndType(tp1: Type, tp2: Type) extends UniqueType {

    type This <: AndType

    def derivedAndType(t1: Type, t2: Type)(implicit ctx: Context) =
      if ((t1 eq tp1) && (t2 eq tp2)) this
      else AndType(t1, t2)

    override def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): Reference =
      (tp1 findMember (name, pre, excluded)) & (tp2 findMember (name, pre, excluded))

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

    override def computeHash = doHash(tp1, tp2)
  }

  final class UniqueOrType(tp1: Type, tp2: Type) extends OrType(tp1, tp2)

  object OrType {
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context) =
      unique(new UniqueOrType(tp1, tp2))
  }

// ----- Method types: MethodType/ExprType/PolyType/MethodParam/PolyParam ---------------

  abstract case class MethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type) extends UniqueType {
    override lazy val resultType = resultTypeExp(this)
    lazy val isDependent = resultType exists {
      case MethodParam(mt, _) => mt eq this
      case _ => false
    }
    def paramSig(tp: Type): TypeName = ???
    lazy val signature: Signature = {
      val sig = paramTypes map paramSig
      resultType match {
        case mt: MethodType => sig ++ mt.signature
        case _ => sig
      }
    }
    def instantiate(argTypes: List[Type])(implicit ctx: Context): Type =
      if (isDependent) new InstMethodMap(this, argTypes) apply resultType
      else resultType
    override def computeHash = doHash(paramNames, resultType, paramTypes)
  }

  final class UniqueMethodType(paramNames: List[TermName], paramTypes: List[Type])(resultTypeExp: MethodType => Type) extends MethodType(paramNames, paramTypes)(resultTypeExp)

  object MethodType {
    def apply(paramNames: List[TermName], paramTypes: List[Type], resultTypeExp: MethodType => Type)(implicit ctx: Context) =
      unique(new UniqueMethodType(paramNames, paramTypes)(resultTypeExp))
  }

  abstract case class ExprType(override val resultType: Type) extends UniqueType {
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
    override lazy val resultType = resultTypeExp(this)

    def instantiate(argTypes: List[Type])(implicit ctx: Context): Type =
      new InstPolyMap(this, argTypes) apply resultType
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
    // need hashCode and equals to be written so that
    // cyclic reference errors are avoided
    override def equals(other: Any) = other match {
      case PolyParam(pt1, pn) => (pt1 eq pt) && (pn == paramNum)
      case _ => false
    }
    override def hashCode = doHash(paramNum)
  }

// ------ Type Bounds ------------------------------------------------------------

  abstract case class TypeBounds(lo: Type, hi: Type) extends UniqueType with TypeProxy {
    def underlying(implicit ctx: Context): Type = hi
    def derivedTypeBounds(lo1: Type, hi1: Type)(implicit ctx: Context) =
      if ((lo1 eq lo) && (hi1 eq hi)) this
      else TypeBounds(lo, hi)

    def & (that: TypeBounds)(implicit ctx: Context): TypeBounds =
      TypeBounds(this.lo | that.lo, this.hi & that.hi)
    def | (that: TypeBounds)(implicit ctx: Context): TypeBounds =
      TypeBounds(this.lo & that.lo, this.hi | that.hi)
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
    override def prefix = NoPrefix
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

    /** Map this function over given type */
    def mapOver(tp: Type): Type = tp match {
      case tp: NamedType   =>
        tp.derivedNamedType(this(tp.prefix), tp.name)

      case ThisType(_)
         | MethodParam(_, _)
         | PolyParam(_, _)
         | ConstantType(_) => tp

      case tp @ AppliedType(tycon, targs) =>
        tp.derivedAppliedType(this(tycon), targs mapConserve this)

      case tp @ PolyType(pnames) =>
        val pbounds = tp.paramBounds
        val pbounds1 = pbounds mapConserve (_ map this)
        val restpe = tp.resultType
        val restpe1 = this(restpe)
        if ((pbounds1 eq pbounds) && (restpe1 eq restpe))
          tp
        else PolyType(pnames)(
          x => pbounds1 mapConserve (_ map (_.subst(tp, x))),
          x => restpe1.subst(tp, x))

      case tp @ MethodType(pnames, ptypes) =>
        val ptypes1 = ptypes mapConserve this
        val restpe = tp.resultType
        val restpe1 = this(restpe)
        if ((ptypes1 eq ptypes) && (restpe1 eq restpe)) tp
        else MethodType(pnames, ptypes1, x => restpe1.subst(tp, x))

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

    private def skipPrefixOf(pre: Type, clazz: Symbol) =
      (pre eq NoType) || (pre eq NoPrefix) || clazz.isPackageClass

    private def toPrefix(pre: Type, clazz: Symbol, thisclazz: ClassSymbol, tp: Type): Type =
      if (skipPrefixOf(pre, clazz))
        tp
      else if ((thisclazz isNonBottomSubClass clazz) &&
        (pre.widen.typeSymbol isNonBottomSubClass thisclazz))
        pre match {
          case SuperType(thistp, _) => thistp
          case _ => pre
        }
      else
        toPrefix(pre.baseType(clazz).prefix, clazz.owner, thisclazz, tp)

    private def toInstance(pre: Type, clazz: Symbol, tparam: Symbol, tp: Type): Type = {
      if (skipPrefixOf(pre, clazz)) tp
      else {
        val tparamOwner = tparam.owner

        def throwError =
          if (tparamOwner.tpe.parents exists (_.isErroneous))
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
        else toInstance(basePre.prefix, clazz.owner, tparam, tp)
      }
    }

    def apply(tp: Type) = tp match {
      case ThisType(thisclazz) =>
        toPrefix(pre, clazz, thisclazz, tp)
      case _ =>
        tp.widen match {
          case tp: TypeRef if tp.typeSymbol.isTypeParameter =>
            toInstance(pre, clazz, tp.typeSymbol, tp)
          case _ =>
            if (tp.isTrivial) tp else mapOver(tp)
        }
    }
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
         | ConstantType(_) => x

      case AppliedType(tycon, targs) =>
        (this(x, tycon) /: targs) (this)

      case tp @ PolyType(pnames) =>
        this((x /: tp.paramBounds) (this), tp.resultType)

      case MethodType(pnames, ptypes) =>
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

// ----- Subtyping -----------------------------------------------------------

  type Constraints = Map[PolyParam, TypeBounds]

  class SubTyper(val constraints: Constraints = Map(), explain: Boolean = false) {

    /**
     *   isSubRefinement(T1, T2) =
     *     val clazz = T1.clazz
     *
     *
     */
    def isSubRefinement(tp1: Type, tp2: Type)(implicit ctx: Context): Boolean = {
      val clazz = tp1.typeSymbol
      assert(clazz eq tp2.typeSymbol)
      if (tp1.prefix != tp2.prefix) return false
      tp1 match {
        case AppliedType(tycon2, args2) =>
          isSubArgs(clazz.typeParams, tp1.typeArgs, args2) &&
          isSubRefinement(tp1, tycon2)
        case RefinedType(tycon2, names) =>
          ???

      }
    }

    def isSubArgs(tparams: List[TypeSymbol], subargs: List[Type], superargs: List[Type]): Boolean = ???

    /**
     *   NoType is not a sub or superType of Anything
     *
     *   T <: ?
     *   ? <: T
     *   T <: Error
     *   Error <: T
     *
     *   T1 <: T2[U2s] :-
     *       T1 <: T2 /\
     *       val T[U1s] = T1 baseType T2.typeSymbol
     *       and for all i:
     *       if !(Ui contravariant in T)      U1i <: U2i
     *       if !(Ui covariant in T)          U2i <: U1i
     *
     *   T1[U1s] <: T2 if
     *       T1 <: T2
     *
     *   P#A <: P#A  if A is no a class, or both types have same symbol
     *
     *   T1 <: T2  if class C = T2.typeSymbol
     *             /\ T1' = t1 baseType C
     *             /\ T1' isSubRefinement T2
     *
     *   T1 <: T2 { Ds }
     *     if T1 <: T2 and forall i, T1 specializesSym Di
     *
     *
     */
    def isSubType(tp1: Type, tp2: Type)(implicit ctx: Context): Boolean = {
???
/*
      if (tp1 == NoType || tp2 == NoType) return false
      if (tp1 eq tp2) return true

      tp2 match {
        case tp2 @ NamedType(pre2, name2) =>
          val ref2 = tp2.referenced
          if (name2.isTerm && ref2.isStable) return isSubType(tp1, ref2.info)
      }

      tp1 match {
        case tp1 @ NamedType(pre1, name1) =>
          val ref1 = tp1.referenced
          val sym1 = ref1.symbol
          if (sym1 != NoSymbol) {
            tp2 match {
              case tp2 @ NamedType(pre2, name2) =>
                val ref2 = tp2.referenced
                val sym2 = ref2.symbol
                if (sym1 eq sym2) return true





                if (sym1.isTerm) {
                  val expanded2 = if (ref2.info.isStable) return ref2.info else tp2
                  return isSubType(ref1.info, expanded2)
                }

            }
          }
      }

      (tp1, tp2) match {
        case (tp1 @ NamedType(pre1, name1), tp2 @ NamedType(pre2, name2)) =>
          if (!(pre1 =:= pre2)) return false
          val ref1 = tp1.referenced
          val ref2 = tp2.referenced
          val sym1 = ref1.symbol
          val sym2 = ref2.symbol
          if (sym1 != NoSymbol && sym2 != NoSymbol) {
            if (sym1 eq sym2) return true
            if (sym2.isTerm && ref2.isStable) return isSubType(tp1, ref2.info)
            if (sym1.isTerm) return isSubType(ref1.info, tp2)
            if (sym1.isClass) {
              if (sym1 == NothingClass) return true
              if (sym2.isClass) return sym1.isNonBottomSubClass(sym2) || sym1 == NullClass
            } else if (isSubType(ref1.upperBound, tp2)) return true
            // !sym1.isClass || !sym2.isClass
            if (name1  == name2) return true
            val lo = ref2.info.lowerBound
            return lo.typeSymbol != NothingClass && isSubType(tp1, lo)




          }
          if (sym1 == NothingClass) return true
          if ()
            if (sym1.isClass)
              if (sym1 == NothingClass) true
              if (sym2.isClass) sym1 isSubClass sym2
              else if (name1 eq name2) true
              else if
          }

            if (name1 == name2)

          }

        (!(tp1.symbol.isClass && tp2.symbol.isClass) || tp1.symbol == tp2.symbol)



      }

    }

    /** First try, on the right:
     *   - unwrap Annotated types, BoundedWildcardTypes,
     *   - bind TypeVars  on the right, if lhs is not Annotated nor BoundedWildcard
     *   - handle common cases for first-kind TypeRefs on both sides as a fast path.
     */
    def firstTry(tp1: Type, tp2: Type)(implicit ctx: Context): Boolean = tp2 match {
      case tp2: NamedType =>
        firstTry(tp1, tp2.underlying)
      case tp2: TypeRef =>

      case tr2: TypeRef =>
        tp1 match {
          case tr1: TypeRef =>
            val sym1 = tr1.sym
            val sym2 = tr2.sym
            val pre1 = tr1.pre
            val pre2 = tr2.pre
            (((if (sym1 == sym2) phase.erasedTypes || sym1.owner.hasPackageFlag || isSubType(pre1, pre2, depth)
               else (sym1.name == sym2.name && !sym1.isModuleClass && !sym2.isModuleClass &&
                     (isUnifiable(pre1, pre2) ||
                      isSameSpecializedSkolem(sym1, sym2, pre1, pre2) ||
                      sym2.isAbstractType && isSubPre(pre1, pre2, sym2)))) &&
                    isSubArgs(tr1.args, tr2.args, sym1.typeParams, depth))
             ||
             sym2.isClass && {
               val base = tr1 baseType sym2
               (base ne tr1) && isSubType(base, tr2, depth)
             }
             ||
             thirdTryRef(tr1, tr2))
          case _ =>
            secondTry
        }
      case AnnotatedType(_, _, _) =>
        isSubType(tp1.withoutAnnotations, tp2.withoutAnnotations, depth) &&
        annotationsConform(tp1, tp2)
      case BoundedWildcardType(bounds) =>
        isSubType(tp1, bounds.hi, depth)
      case tv2 @ TypeVar(_, constr2) =>
        tp1 match {
          case AnnotatedType(_, _, _) | BoundedWildcardType(_) =>
            secondTry
          case _ =>
            tv2.registerBound(tp1, true)
        }
      case _ =>
        secondTry*/
    }


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
}