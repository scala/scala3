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
import RefSets._
import Periods._
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

  /** The signature of a reference.
   *  Overloaded references with the same name are distinguished by
   *  their signatures. A signature is a list of the fully qualified names
   *  of the type symbols of the erasure of the parameters of the
   *  reference. For instance a reference to the definition
   *
   *      def f(x: Int)(y: List[String]): String
   *
   *  would have signature
   *
   *      List("scala.Int".toTypeName, "scala.collection.immutable.List".toTypeName)
   */
  type Signature = List[TypeName]

  val NullSignature = List(Names.EmptyTypeName)

  /** The variants constituting an overloaded reference.
   *  This is a set of non-overloaded references indexed by their
   *  signatures. All instances of Variants are assumed to
   *  have NoSymbol as default value.
   */
  type Variants = Map[Signature, RefType]

  /** The canonical creator of variants maps.
   *  Note that it adds NoSymbol as default value.
   */
  def Variants(bindings: (Signature, RefType)*): Variants =
    Map(bindings: _*) withDefaultValue NoType

  abstract class Type {

    def <:< (that: Type): Boolean = ???

    def hash = NotCached

    /** The type symbol associated with the type
     */
    def typeSymbol: Symbol = NoSymbol

    /** The term symbol associated with the type
     */
    def termSymbol: Symbol = NoSymbol

    /** Does this type denote a stable reference (i.e. singleton type)? */
    def isStableType: Boolean = false

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
    def parents: List[Type] = List()

    def bounds(implicit ctx: Context): TypeBounds = TypeBounds(this, this)

    def decl(name: Name)(implicit ctx: Context): RefType =
      findClassDecl(name, typeSymbol.thisType, Flags.Empty)

    // need: NoSymbol is as good as any other
    def isAsGood(tp1: Type, tp2: Type)(implicit ctx: Context): Boolean = ???

    def member(name: Name)(implicit ctx: Context): Type =
      findMember(name, this, Flags.Empty)

    def nonPrivateMember(name: Name)(implicit ctx: Context): Type =
      findMember(name, this, Flags.Private)

    def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): RefType =
      throw new AssertionError(s"cannot find members of $this")

    protected def findClassMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): RefType =
      findMemberAmong(typeSymbol.asClass.deref.memberRefsNamed(name), pre, excluded)

    protected def findClassDecl(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): RefType =
      findMemberAmong(typeSymbol.asClass.deref.declsNamed(name), pre, excluded)

    private def findMemberAmong(candidates: RefSet, pre: Type, excluded: FlagSet)(implicit ctx: Context): RefType = {
      val resultSyms = candidates.filterAccessibleFrom(pre).filterExcluded(excluded)
      def makeRef(refs: RefSet): RefType = refs match {
        case RefUnion(refs1, refs2) => makeRef(refs1) ref_& makeRef(refs2)
        case ref: SymRef => ref
      }
      if (resultSyms.isEmpty) ErrorRefType // todo: refine
      else makeRef(resultSyms)
    }

    def memberType(sym: Symbol): Type = ???
    def memberInfo(sym: Symbol): Type = ???

    def widen: Type = ???

    def deconst: Type = ???

    def prefix(implicit ctx: Context): Type = ???

    def isTrivial: Boolean = ???

    def resultType: Type = ???

    def isCachable: Boolean = false

    def asSeenFrom(pre: Type, clazz: Symbol)(implicit ctx: Context): Type =
      if (this.isTrivial || clazz.isStaticMono) this
      else new AsSeenFromMap(pre, clazz) apply (this)

    def subst(from: List[Symbol], to: List[Type]): Type = ???
    def subst(from: PolyType, to: PolyType): Type = ???
    def subst(from: MethodType, to: MethodType): Type = ???
    def substSym(from: List[Symbol], to: List[Symbol]): Type = ???

    def baseType(clazz: Symbol): Type = ???

    def typeParams: List[TypeSymbol] = ???

    def effectiveBounds: TypeBounds = ???
    def isWrong: Boolean = ???

    def & (that: Type)(implicit ctx: Context): Type =
      if (this eq that) this
      else if (this.isWrong) that
      else if (that.isWrong) this
      else (this, that) match {
        case (_, OrType(that1, that2)) =>
          this & that1 | this & that2
        case (OrType(this1, this2), _) =>
          this1 & that | this2 & that
        case _ =>
          val t1 = lower(this, that)
          if (t1 ne that) t1
          else {
            val t2 = lower(that, this)
            if (t2 ne this) t2
            else AndType(this, that)
          }
      }

   def | (that: Type)(implicit ctx: Context): Type =
      if (this eq that) this
      else if (this.isWrong) this
      else if (that.isWrong) that
      else {
        val t1 = higher(this, that)
        if (t1 ne that) t1
        else {
          val t2 = higher(that, this)
          if (t2 ne this) t2
          else {
            val t1w = t1.widen
            val t2w = t2.widen
            if ((t1w ne t1) || (t2w ne t2)) t1w | t2w
            else OrType(this, that)
          }
        }
      }

   private def lower(t1: Type, t2: Type)(implicit ctx: Context): Type =
      if (t1 <:< t2) t1
      else t2 match {
        case t2 @ AndType(t21, t22) => t2.derivedAndType(lower(t1, t21), lower(t1, t22))
        case _ => t2
      }

    private def higher(t1: Type, t2: Type)(implicit ctx: Context): Type =
      if (t2 <:< t1) t1
      else t2 match {
        case t2 @ OrType(t21, t22) => t2.derivedOrType(higher(t1, t21), higher(t1, t22))
        case _ => t2
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

    protected def doHash(variants: Variants): Int = {
      var h = hashSeed
      val it = variants.valuesIterator
      while (it.hasNext) {
        val elemHash = it.next.hash
        if (elemHash == NotCached) return NotCached
        h = hashing.mix(h, elemHash)
      }
      finishHash(h, variants.size)
    }

    protected def doHash(x1: Any, tp2: Type, tps3: List[Type]): Int =
      finishHash(hashing.mix(hashSeed, x1.hashCode), 1, tp2, tps3)
  } // end Type

  abstract class UniqueType extends Type {
    final override val hash = computeHash
    def computeHash: Int
  }

  def unique[T <: Type](tp: T)(implicit ctx: Context): T = {
    if (tp.hash == NotCached) tp
    else ctx.root.uniques.findEntryOrUpdate(tp).asInstanceOf[T]
  }


  trait RefType extends Type {
    def signature(implicit ctx: Context): Signature =
      throw new UnsupportedOperationException(this.getClass+".signature")

    def orElse(alt: => RefType): RefType = if (isWrong) alt else this

    def ref_& (that: RefType)(implicit ctx: Context): RefType =
      if (this eq that) this
      else if (this.isWrong) that
      else if (that.isWrong) this
      else (this, that) match {
        case (OverloadedType(vs1), OverloadedType(vs2)) =>
          OverloadedType(conj(vs1, vs2))
        case (_, OverloadedType(vs2)) =>
          OverloadedType(vs2 updated (this.signature, this ref_& vs2(this.signature)))
        case (OverloadedType(vs1), _) =>
          OverloadedType(vs1 updated (that.signature, vs1(that.signature) ref_& that))
        case (this1 @ TypeRef(pre1, sym1), that1 @ TypeRef(pre2, sym2)) =>
          val rawtpe =
            if (sym1.isAbstractType) this1
            else if (sym2.isAbstractType) that1
            else
              TypeRef(pre1, sym1.owner.newAbstractType(sym1.name.asTypeName, sym1.info.bounds))
          rawtpe withInfo (this1.bounds & that1.bounds)
        case (this1 @ TermRef(pre1, sym1), that1 @ TermRef(pre2, sym2)) =>
          if ((this.signature != that.signature))
            OverloadedType(Variants(this.signature -> this, that.signature -> that))
          else
            TermRef(this1.prefix, sym1, this1.info & that1.info)
      }

    def ref_| (that: RefType)(implicit ctx: Context): RefType =
      if (this eq that) this
      else if (this.isWrong) this
      else if (that.isWrong) that
      else (this, that) match {
        case (OverloadedType(vs1), OverloadedType(vs2)) =>
          OverloadedType(disj(vs1, vs2))
        case (_, OverloadedType(vs2)) =>
          this ref_| vs2(this.signature)
        case (OverloadedType(vs1), _) =>
          vs1(that.signature) ref_| that
        case (this1 @ TypeRef(pre1, sym1), that1 @ TypeRef(pre2, sym2)) =>
          val rbounds = this1.bounds | that1.bounds
          val rsym = lubSym(pre1, sym1, sym2) orElse {
            val rpre = RefinedType(pre1, newScope)
            val rsym = rpre.typeSymbol.newAbstractType(sym1.name, rbounds)
            rpre.decls.enter(rsym)
            rsym
          }
          TypeRef(pre1, rsym.asType)
        case (this1 @ TermRef(pre1, sym1), that1 @ TermRef(pre2, sym2)) =>
          val rtpe = this1.info | that1.info
          val rsym = lubSym(pre1, sym1, sym2) orElse {
            val rpre = RefinedType(pre1, newScope)
            val rsym = rpre.typeSymbol.newAbstractTerm(sym1.name, rtpe)
            rpre.decls.enter(rsym)
            rsym
          }
          TermRef(pre1, rsym.asTerm)
      }

    /** Conjunction of two variants sets */
    private def conj(vs1: Variants, vs2: Variants)(implicit ctx: Context): Variants =
      Variants(
        ((vs1.keySet | vs2.keySet) map (sig => (sig -> (vs1(sig) ref_& vs2(sig))))).toSeq: _*)

    /** Disjunction of two variants sets */
    private def disj(vs1: Variants, vs2: Variants)(implicit ctx: Context): Variants =
      Variants(
        ((vs1.keySet & vs2.keySet) map (sig => (sig -> (vs1(sig) ref_| vs2(sig))))).toSeq: _*)

    private def lubSym(pre: Type, sym1: Symbol, sym2: Symbol)(implicit ctx: Context): Symbol = {
      def qualifies(sym: Symbol) =
        (sym isAccessibleFrom pre) && (sym2.owner isSubClass sym.owner)
      sym1.allOverriddenSymbols find qualifies getOrElse NoSymbol
    }
  }

  case class OverloadedType(variants: Variants) extends UniqueType with RefType {
    override def computeHash: Int = doHash(variants)
  }

  abstract class SymRef extends SubType with RefType with RefSetSingleton {
    def prefix: Type
    def symbol: Symbol

    def isType: Boolean = symbol.isType
    def isTerm = !isType

    protected var infoVar: Type = null

    def info(implicit ctx: Context): Type = {
      if (infoVar == null) infoVar = symbol.info.asSeenFrom(prefix, symbol.owner)
        infoVar
    }

    def widen(implicit ctx: Context): Type = if (symbol.isTerm) info.widen else this

    def derivedSymRef(pre: Type, sym: Symbol)(implicit ctx: Context): Type =
      if (pre eq prefix)
        this
      else if (sym.isOverridable && (sym.owner ne pre.typeSymbol))
        pre.nonPrivateMember(sym.name)
      else
        SymRef(pre, sym)

    def underlying(implicit ctx: Context) = info

    override def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): RefType =
      if (symbol.isClass) findClassMember(name, pre, excluded)
      else info.findMember(name, pre, excluded)

    def withInfo(newinfo: Type)(implicit ctx: Context): SymRef =
     if (info eq newinfo) this else SymRef(prefix, symbol, info)

    override def computeHash = doHash(symbol, prefix)
  }

  abstract case class TypeRef(prefix: Type, symbol: TypeSymbol) extends SymRef {
    override def typeSymbol = symbol
    override def signature(implicit ctx: Context): Signature = NullSignature
  }

  final class UniqueTypeRef(prefix: Type, symbol: TypeSymbol) extends TypeRef(prefix, symbol)

  abstract case class TermRef(prefix: Type, symbol: TermSymbol) extends SymRef {
    override def termSymbol = symbol
    private var signatureVar: Signature = null
    private var signatureRun: RunId = NoRunId
    override def signature(implicit ctx: Context): Signature = {
      def paramSig(tp: Type): TypeName = ???
      def resultSig(tp: Type): Signature = {
        val s = sig(tp)
        if (s eq NullSignature) Nil else s
      }
      def sig(tp: Type): Signature = tp match {
        case tp: PolyType =>
          resultSig(tp.resultType)
        case tp: MethodType =>
          (tp.paramTypes map paramSig) ::: resultSig(tp.resultType)
        case _ => NullSignature
      }
      if (signatureRun != ctx.runId) {
        signatureVar = sig(info)
        signatureRun = ctx.runId
      }
      signatureVar
    }
  }

  final class UniqueTermRef(prefix: Type, symbol: TermSymbol) extends TermRef(prefix, symbol)

  object SymRef {
    def apply(prefix: Type, sym: Symbol)(implicit ctx: Context): SymRef =
      if (sym.isType) TypeRef(prefix, sym.asType) else TermRef(prefix, sym.asTerm)
    def apply(prefix: Type, sym: Symbol, info: Type)(implicit ctx: Context): SymRef = {
      if (sym.isType) TypeRef(prefix, sym.asType, info) else TermRef(prefix, sym.asTerm, info)
    }
  }

  object TypeRef {
    def apply(prefix: Type, sym: TypeSymbol)(implicit ctx: Context): TypeRef =
      unique(new UniqueTypeRef(prefix, sym))
    def apply(prefix: Type, sym: TypeSymbol, info: Type)(implicit ctx: Context): TypeRef = {
      val ref = apply(prefix, sym)
      ref.infoVar = info
      ref
    }
  }

  object TermRef {
    def apply(prefix: Type, sym: TermSymbol)(implicit ctx: Context): TermRef =
      unique(new UniqueTermRef(prefix, sym))
    def apply(prefix: Type, sym: TermSymbol, info: Type)(implicit ctx: Context): TermRef = {
      val ref = apply(prefix, sym)
      ref.infoVar = info
      ref
    }

    def make(pre: Type, sym: TermSymbol)(implicit ctx: Context) =
      if (ctx.phase.erasedTypes) sym.tpe.resultType
      else TermRef(pre, sym) // was more complicated; see singleType
  }

  abstract case class AppliedType(tycon: Type, typeArgs: List[Type]) extends UniqueType {
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

  abstract case class AndType(tp1: Type, tp2: Type) extends UniqueType {
    def derivedAndType(t1: Type, t2: Type)(implicit ctx: Context) =
      if ((t1 eq tp1) && (t2 eq tp2)) this
      else AndType(tp1, tp2)

    override def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): RefType =
      (tp1 findMember (name, pre, excluded)) ref_& (tp2 findMember (name, pre, excluded))

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
      else OrType(tp1, tp2)

    override def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): RefType =
      tp1.findMember(name, pre, excluded) ref_| tp2.findMember(name, pre, excluded)

    override def computeHash = doHash(tp1, tp2)
  }

  final class UniqueOrType(tp1: Type, tp2: Type) extends OrType(tp1, tp2)

  object OrType {
    def apply(tp1: Type, tp2: Type)(implicit ctx: Context) =
      unique(new UniqueOrType(tp1, tp2))
  }

  case object NoType extends SymRef {
    def prefix = NoPrefix
    def symbol = NoSymbol
  }

  case object NoPrefix extends UniqueType {
    override def computeHash = hashSeed
  }

  abstract class ErrorType extends Type

  object ErrorType extends ErrorType
  object ErrorRefType extends ErrorType with RefType

  trait SubType extends UniqueType {
    def underlying(implicit ctx: Context): Type
  }

  abstract class SingletonType extends SubType

  abstract case class ThisType(clazz: ClassSymbol) extends SingletonType {
    def underlying(implicit ctx: Context) = clazz.typeOfThis
    override def computeHash = doHash(clazz)
  }

  final class UniqueThisType(clazz: ClassSymbol) extends ThisType(clazz)

  object ThisType {
    def apply(clazz: ClassSymbol)(implicit ctx: Context) =
      unique(new UniqueThisType(clazz))
  }

  abstract case class SuperType(thistpe: Type, supertpe: Type) extends SingletonType {
    def derivedSuperType(thistp: Type, supertp: Type)(implicit ctx: Context) =
      if ((thistp eq thistpe) && (supertp eq supertpe)) this
      else SuperType(thistp, supertp)
    def underlying(implicit ctx: Context) = supertpe
    override def computeHash = doHash(thistpe, supertpe)
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

  case class RefinedType(parent: Type, decls: Scope) extends Type { // can make uniquetype ??? but need to special-case symbols
    def derivedRefinedType(parent1: Type, decls1: Scope): RefinedType =
      if ((parent1 eq parent) && (decls1 eq decls)) this
      else RefinedType(parent1, decls1)

    override def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): RefType =
      findClassDecl(name, pre, excluded) orElse parent.findMember(name, pre, excluded)
  }

  abstract case class MethodType(paramNames: List[TermName], paramTypes: List[Type], resultTypeExp: MethodType => Type) extends UniqueType {
    override lazy val resultType = resultTypeExp(this)
    lazy val isDependent = resultType exists {
      case MethodParam(mt, _) => mt eq this
      case _ => false
    }
    def instantiate(argTypes: List[Type])(implicit ctx: Context): Type =
      if (isDependent) new InstMethodMap(this, argTypes) apply resultType
      else resultType
    override def computeHash = doHash(paramNames, resultType, paramTypes)
  }

  final class UniqueMethodType(paramNames: List[TermName], paramTypes: List[Type], resultTypeExp: MethodType => Type) extends MethodType(paramNames, paramTypes, resultTypeExp)

  object MethodType {
    def apply(paramNames: List[TermName], paramTypes: List[Type], resultTypeExp: MethodType => Type)(implicit ctx: Context) =
      unique(new UniqueMethodType(paramNames, paramTypes, resultTypeExp))
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

  case class PolyType(paramNames: List[TypeName], paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => Type) extends Type {
    lazy val paramBounds = paramBoundsExp(this)
    override lazy val resultType = resultTypeExp(this)

    def derivedPolyType(pnames: List[TypeName], pboundsExp: PolyType => List[TypeBounds], restpeExp: PolyType => Type): Type = {
      val restpe = PolyType(pnames, pboundsExp, restpeExp)
      if ((pnames eq paramNames) &&
          (pboundsExp(this) eq paramBounds) &&
          (restpeExp(this) eq resultType)) this
      else restpe
    }
    def instantiate(argTypes: List[Type])(implicit ctx: Context): Type =
      new InstPolyMap(this, argTypes) apply resultType
  }

  case class MethodParam(mt: MethodType, paramNum: Int) extends SingletonType {
    def underlying(implicit ctx: Context) = mt.paramTypes(paramNum)
    override def computeHash = NotCached
  }

  case class PolyParam(pt: PolyType, paramNum: Int) extends Type {
  }

  abstract case class TypeBounds(lo: Type, hi: Type) extends UniqueType {
    def derivedTypeBounds(lo1: Type, hi1: Type)(implicit ctx: Context) =
      if ((lo1 eq lo) && (hi1 eq hi)) this
      else TypeBounds(lo, hi)

    def & (that: TypeBounds)(implicit ctx: Context): TypeBounds =
      TypeBounds(this.lo | that.lo, this.hi & that.hi)
    def | (that: TypeBounds)(implicit ctx: Context): TypeBounds =
      TypeBounds(this.lo & that.lo, this.hi | that.hi)
    override def findMember(name: Name, pre: Type, excluded: FlagSet)(implicit ctx: Context): RefType =
      hi.findMember(name, pre, excluded)
    def map(f: Type => Type)(implicit ctx: Context): TypeBounds =
      TypeBounds(f(lo), f(hi))
    override def computeHash = doHash(lo, hi)
  }

  final class UniqueTypeBounds(lo: Type, hi: Type) extends TypeBounds(lo, hi)

  object TypeBounds {
    def apply(lo: Type, hi: Type)(implicit ctx: Context) =
      unique(new UniqueTypeBounds(lo, hi))
  }

  case class AnnotatedType(annots: List[AnnotationInfo], underlying: Type) extends Type {
    def derivedAnnotatedType(annots1: List[AnnotationInfo], underlying1: Type) =
      if ((annots1 eq annots) && (underlying1 eq underlying)) this
      else AnnotatedType.make(annots1, underlying1)
  }

  object AnnotatedType {
    def make(annots: List[AnnotationInfo], underlying: Type) =
      if (annots.isEmpty) underlying
      else AnnotatedType(annots, underlying)
  }

  abstract class TypeMap(implicit ctx: Context) extends (Type => Type) {
    def apply(tp: Type): Type

    /** Map this function over given type */
    def mapOver(tp: Type): Type = tp match {
      case tp: SymRef =>
        tp.derivedSymRef(this(tp.prefix), tp.symbol)

      case ThisType(_)
         | MethodParam(_, _)
         | PolyParam(_, _)
         | ConstantType(_) => tp

      case tp @ AppliedType(tycon, targs) =>
        tp.derivedAppliedType(this(tycon), targs mapConserve this)

      case tp @ PolyType(pnames, pboundsExpr, restpeExpr) =>
        val pbounds = tp.paramBounds
        val pbounds1 = pbounds mapConserve (_ map this)
        val restpe = tp.resultType
        val restpe1 = this(restpe)
        if ((pbounds1 eq pbounds) && (restpe1 eq restpe))
          tp
        else PolyType(
          pnames,
          x => pbounds1 mapConserve (_ map (_.subst(tp, x))),
          x => restpe1.subst(tp, x))

      case tp @ MethodType(pnames, ptypes, restpeExpr) =>
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

      case tp @ RefinedType(parent, decls) =>
        tp.derivedRefinedType(this(parent), mapOver(decls))

      case tp @ OverloadedType(vs) =>
        val altTypes = (vs map (_._2)).toList
        val altTypes1 = altTypes mapConserve (t => this(t).asInstanceOf[RefType])
        if (altTypes eq altTypes1) tp
        else altTypes.reduceLeft(_ ref_& _)

      case tp @ AnnotatedType(annots, underlying) =>
        tp.derivedAnnotatedType(mapOverAnnotations(annots), this(underlying))

      case _ =>
        tp
    }

    /** Map this function over given scope */
    def mapOver(scope: Scope): Scope = {
      val elems = scope.toList
      val elems1 = mapOver(elems)
      if (elems1 eq elems) scope
      else newScopeWith(elems1: _*)
    }

    /** Map this function over given list of symbols */
    def mapOver(syms: List[Symbol]): List[Symbol] = {
      val infos = syms map (_.info)
      val infos1 = infos mapConserve this
      if (infos eq infos1) syms
      else {
        val syms1 = syms map (_.cloneSymbol)
        (syms1, infos1).zipped.foreach { (sym1, info1) =>
          sym1 setDenotation sym1.deref.withType(info1 substSym (syms, syms1))
        }
        syms1
      }
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

//  Constraints: poly => tvars
//



  class AsSeenFromMap(pre: Type, clazz: Symbol)(implicit ctx: Context) extends TypeMap {
    private def skipPrefixOf(pre: Type, clazz: Symbol) =
      (pre eq NoType) || (pre eq NoPrefix) || clazz.isPackageClass
    def apply(tp: Type) = tp match {
      case ThisType(sym) =>
        def toPrefix(pre: Type, clazz: Symbol): Type =
          if (skipPrefixOf(pre, clazz))
            tp
          else if ((sym isNonBottomSubClass clazz) &&
            (pre.widen.typeSymbol isNonBottomSubClass sym))
            pre match {
              case SuperType(thistp, _) => thistp
              case _ => pre
            }
          else
            toPrefix(pre.baseType(clazz).prefix, clazz.owner)
        toPrefix(pre, clazz)
      case TypeRef(_, tparam) if tparam.isTypeParameter =>
        def toInstance(pre: Type, clazz: Symbol): Type = {
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
            else toInstance(basePre.prefix, clazz.owner)
          }
        }
        toInstance(pre, clazz)
      case _ =>
        if (tp.isTrivial) tp else mapOver(tp)
    }
  }

  abstract class TypeAccumulator[T] extends ((T, Type) => T) {
    def apply(x: T, tp: Type): T

    def apply(x: T, sym: Symbol): T = apply(x, sym.info(NoContext))

    def apply(x: T, annot: AnnotationInfo): T = ???

    def foldOver(x: T, tp: Type): T = tp match {
      case tp: SymRef =>
        this(x, tp.prefix)

      case ThisType(_)
         | MethodParam(_, _)
         | PolyParam(_, _)
         | ConstantType(_) => x

      case AppliedType(tycon, targs) =>
        (this(x, tycon) /: targs) (this)

      case tp @ PolyType(pnames, pboundsExpr, restpeExpr) =>
        this((x /: tp.paramBounds) (this), tp.resultType)

      case MethodType(pnames, ptypes, restpeExpr) =>
        this((x /: ptypes) (this), tp.resultType)

      case ExprType(restpe) =>
        this(x, restpe)

      case SuperType(thistp, supertp) =>
        this(this(x, thistp), supertp)

      case TypeBounds(lo, hi) =>
        this(this(x, lo), hi)

      case RefinedType(parent, decls) =>
        (this(x, parent) /: decls.toList) (apply)

      case OverloadedType(vs) =>
        (x /: (vs map (_._2))) (this)

      case AnnotatedType(annots, underlying) =>
        this((x /: annots) (apply), underlying)

      case _ => x

    }

  }

  class ExistsAccumulator(p: Type => Boolean) extends TypeAccumulator[Boolean] {
    def apply(x: Boolean, tp: Type) = x || p(tp) || foldOver(x, tp)
  }

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

  final val NotCached = 0
  final val NotCachedAlt = Int.MinValue

  /** Compute the hash of a product
  def productHash(x: Product): Int = {
    val arr = x.productArity
    var h = x.productPrefix.hashCode
    var i = 0
    while (i < arr) {
      val elemHash = x.productElement(i) match {
        case tp: Type => tp.hash
        case elem => elem.hashCode
      }
      if (elemHash == NotCached) return NotCached
      h = hashing.mix(h, elemHash)
      i += 1
    }
    finalizeHash(h, arr)
  }*/


}