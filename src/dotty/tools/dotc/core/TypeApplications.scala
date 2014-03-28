package dotty.tools.dotc
package core

import Types._
import Contexts._
import Symbols._
import Decorators._
import util.Stats._
import util.common._
import Names._
import Flags._
import util.Positions.Position
import config.Printers._
import collection.mutable

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
}

import TypeApplications._

/** A decorator that provides methods for modeling type application */
class TypeApplications(val self: Type) extends AnyVal {

  def canHaveTypeParams(implicit ctx: Context) = !ctx.erasedTypes || self.isRef(defn.ArrayClass)

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
  final def typeParams(implicit ctx: Context): List[TypeSymbol] = /*>|>*/ track("typeParams") /*<|<*/ {
    self match {
      case tp: ClassInfo =>
        tp.cls.typeParams
      case tp: TypeRef =>
        val tsym = tp.typeSymbol
        if (tsym.isClass) tsym.typeParams
        else if (tsym.info.isAlias) tp.underlying.typeParams
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
  }
  /** The type parameters of the underlying class.
   *  This is like `typeParams`, except for 3 differences.
   *  First, it does not adjust type parameters in refined types. I.e. type arguments
   *  do not remove corresponding type parameters.
   *  Second, it will return Nil for BoundTypes because we might get a NullPointer exception
   *  on PolyParam#underlying otherwise (demonstrated by showClass test).
   *  Third, it won't return higher-kinded type parameters.
   */
  final def safeUnderlyingTypeParams(implicit ctx: Context): List[TypeSymbol] = {
    def ifCompleted(sym: Symbol): Symbol = if (sym.isCompleted) sym else NoSymbol
    self match {
      case tp: ClassInfo =>
        tp.cls.typeParams
      case tp: TypeRef =>
        val tsym = tp.typeSymbol
        if (tsym.isClass) tsym.typeParams
        else if (tsym.isAliasType) tp.underlying.safeUnderlyingTypeParams
        else Nil
      case tp: BoundType =>
        Nil
      case tp: TypeProxy =>
        tp.underlying.safeUnderlyingTypeParams
      case tp: AndType =>
        tp.tp1.safeUnderlyingTypeParams
      case _ =>
        Nil
    }
  }

  def uninstantiatedTypeParams(implicit ctx: Context): List[TypeSymbol] =
    typeParams filter (tparam => self.member(tparam.name).symbol == tparam)

  /** Encode the type resulting from applying this type to given arguments */
  final def appliedTo(args: List[Type])(implicit ctx: Context): Type = /*>|>*/ track("appliedTo") /*<|<*/ {

    def recur(tp: Type, tparams: List[TypeSymbol], args: List[Type]): Type = args match {
      case arg :: args1 =>
        if (tparams.isEmpty) {
          println(s"applied type mismatch: $self $args, typeParams = $typeParams, tsym = ${self.typeSymbol.debugString}") // !!! DEBUG
          println(s"precomplete decls = ${self.typeSymbol.decls.toList.map(_.denot).mkString("\n  ")}")
        }
        val tparam = tparams.head
        val tp1 = RefinedType(tp, tparam.name, arg.toBounds(tparam))
        recur(tp1, tparams.tail, args1)
      case nil => tp
    }

    def safeTypeParams(tsym: Symbol) =
      if (tsym.isClass || !self.typeSymbol.isCompleting) typeParams
      else {
        ctx.warning("encountered F-bounded higher-kinded type parameters; assuming they are invariant")
        defn.hkTrait(args map alwaysZero).typeParams
      }

    if (args.isEmpty || !canHaveTypeParams) self
    else self match {
      case tp: TypeRef =>
        val tsym = tp.symbol
        if (tsym.isAliasType) tp.underlying.appliedTo(args)
        else recur(tp, safeTypeParams(tsym), args)
      case tp: TypeProxy =>
        tp.underlying.appliedTo(args)
      case AndType(l, r) =>
        l.appliedTo(args) & r
      case tp: PolyType =>
        tp.instantiate(args)
      case ErrorType =>
        self
    }
  }

  final def appliedTo(arg: Type)(implicit ctx: Context): Type = appliedTo(arg :: Nil)
  final def appliedTo(arg1: Type, arg2: Type)(implicit ctx: Context): Type = appliedTo(arg1 :: arg2 :: Nil)

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
      base.typeParams map (param => self.member(param.name).info.argInfo(param))
    else
      Nil

  /** The type arguments of this type's base type instance wrt.`base`.
   *  Existential types in arguments are disallowed.
   */
  final def baseArgTypes(base: Symbol)(implicit ctx: Context): List[Type] =
    baseArgInfos(base) mapConserve noBounds

  /** The type arguments of this type's base type instance wrt.`base`.
   *  Existential types in arguments are approximanted by their lower bound.
   */
  final def baseArgTypesLo(base: Symbol)(implicit ctx: Context): List[Type] =
    baseArgInfos(base) mapConserve boundsToLo

  /** The type arguments of this type's base type instance wrt.`base`.
   *  Existential types in arguments are approximanted by their upper bound.
   */
  final def baseArgTypesHi(base: Symbol)(implicit ctx: Context): List[Type] =
    baseArgInfos(base) mapConserve boundsToHi

  /** The first type argument of the base type instance wrt `base` of this type */
  final def firstBaseArgInfo(base: Symbol)(implicit ctx: Context): Type = base.typeParams match {
    case param :: _ if self derivesFrom base =>
      self.member(param.name).info.argInfo(param)
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
        val pbase = parent.baseTypeWithArgs(base)
        if (pbase.member(name).exists) RefinedType(pbase, name, tp.refinedInfo)
        else pbase
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
   */
  def translateParameterized(from: ClassSymbol, to: ClassSymbol)(implicit ctx: Context): Type =
    if (self.derivesFrom(from))
      if (canHaveTypeParams)
        RefinedType(to.typeRef, to.typeParams.head.name, self.member(from.typeParams.head.name).info)
      else
        to.typeRef
    else self

  /** If this is an encoding of a (partially) applied type, return its arguments,
   *  otherwise return Nil.
   *  Existential types in arguments are returned as TypeBounds instances.
   */
  final def argInfos(implicit ctx: Context): List[Type] = {
    var tparams: List[TypeSymbol] = null
    def recur(tp: Type, refineCount: Int): mutable.ListBuffer[Type] = tp.stripTypeVar match {
      case tp @ RefinedType(tycon, name) =>
        val buf = recur(tycon, refineCount + 1)
        if (buf == null) null
        else {
          if (tparams == null) tparams = tycon.typeParams
          if (buf.size < tparams.length) {
            val tparam = tparams(buf.size)
            if (name == tparam.name) buf += tp.refinedInfo.argInfo(tparam)
            else null
          } else null
        }
      case _ =>
        if (refineCount == 0) null
        else new mutable.ListBuffer[Type]
    }
    val buf = recur(self, 0)
    if (buf == null) Nil else buf.toList
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

  /** If this is the image of a type argument to type parameter `tparam`,
   *  recover the type argument, otherwise NoType.
   */
  final def argInfo(tparam: Symbol)(implicit ctx: Context): Type = self match {
    case TypeBounds(lo, hi) =>
      if (lo eq hi) hi
      else {
        val v = tparam.variance
        if (v > 0 && (lo isRef defn.NothingClass)) hi
        else if (v < 0 && (hi isRef defn.AnyClass)) lo
        else self // it's wildcard type; return its bounds
      }
    case _ =>
      NoType
  }

  /** The element type of a sequence or array */
  def elemType(implicit ctx: Context): Type =
    firstBaseArgInfo(defn.SeqClass) orElse firstBaseArgInfo(defn.ArrayClass)

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
  def LambdaAbstract(boundSyms: List[Symbol])(error: (String, Position) => Unit)(implicit ctx: Context): Type = self match {
    case AndType(l, r) =>
      AndType(l.LambdaAbstract(boundSyms)(error), r)
    case _ =>
      val cls = self.typeSymbol
      if (!cls.isClass)
        error("right-hand side of parameterized alias type must refer to a class", cls.pos)

      val correspondingParamName: Map[Symbol, TypeName] = {
        for {
          (tparam, targ: TypeRef) <- cls.typeParams zip argInfos
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

      rewrite(self)
  }
}