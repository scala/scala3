package scala.quoted.compiletime.internal

import dotty.tools.dotc
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.*
import scala.quoted.compiletime.internal.casting.*
import scala.quoted.compiletime as pub

/////// TypeRepr ///////////////////////////////////////////////////////////////

type TypeRepr = pub.TypeRepr & TypeReprImpl
sealed trait TypeReprImpl(using val ctx: Context) { _self: pub.TypeRepr =>

  val underlying: dotc.core.Types.Type

  override final def toString: String = underlying.toString
  override final def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: TypeReprImpl => this.underlying == that.underlying
    case _                  => false

  def asType: scala.quoted.Type[?] =
    new scala.quoted.runtime.impl.TypeImpl(tpd.TypeTree(underlying), scala.quoted.runtime.impl.SpliceScope.getCurrent)

  def =:=(that: pub.TypeRepr): Boolean = underlying =:= that.cast.underlying
  def <:<(that: pub.TypeRepr): Boolean = underlying <:< that.cast.underlying
  def widen: TypeRepr = TypeReprImpl(underlying.widen)
  def widenTermRefByName: TypeRepr =
    val res = underlying.widenTermRefExpr match
      case dotc.core.Types.ClassInfo(prefix, sym, _, _, _) => prefix.select(sym)
      case other                                           => other
    TypeReprImpl(res)
  def widenByName: TypeRepr = TypeReprImpl(underlying.widenExpr)
  def dealias: TypeRepr = TypeReprImpl(underlying.dealias)
  def dealiasKeepOpaques: TypeRepr = TypeReprImpl(underlying.dealiasKeepOpaques)
  def simplified: TypeRepr = TypeReprImpl(underlying.simplified)
  def classSymbol: Option[SymbolImpl] =
    if underlying.classSymbol.exists then Some(SymbolImpl(underlying.classSymbol.asClass))
    else None
  def typeSymbol: SymbolImpl = SymbolImpl(underlying.typeSymbol)
  def termSymbol: SymbolImpl = SymbolImpl(underlying.termSymbol)
  def isSingleton: Boolean = underlying.isSingleton
  def memberType(member: pub.Symbol): TypeRepr =
    // we replace thisTypes here to avoid resolving otherwise unstable prefixes into Nothing
    val memberInfo =
      if underlying.typeSymbol.isClassDef then member.cast.symbol.info.substThis(underlying.classSymbol.asClass, underlying)
      else member.cast.symbol.info
    TypeReprImpl(memberInfo.asSeenFrom(underlying, member.cast.symbol.owner))

  def baseClasses: List[SymbolImpl] = underlying.baseClasses.map(SymbolImpl(_))
  def baseType(cls: pub.Symbol): TypeRepr = TypeReprImpl(underlying.baseType(cls.cast.symbol))
  def derivesFrom(cls: pub.Symbol): Boolean = underlying.derivesFrom(cls.cast.symbol)
  def isFunctionType: Boolean =
    dotc.core.Symbols.defn.isFunctionNType(underlying)
  def isContextFunctionType: Boolean =
    dotc.core.Symbols.defn.isContextFunctionType(underlying)
  def isErasedFunctionType: Boolean =
    underlying match
      case dotc.core.Symbols.defn.PolyFunctionOf(mt) =>
        mt match
          case mt: dotc.core.Types.MethodType      => mt.hasErasedParams
          case dotc.core.Types.PolyType(_, _, mt1) => mt1.hasErasedParams
      case _ => false
  def isDependentFunctionType: Boolean =
    val tpNoRefinement = underlying.dropDependentRefinement
    tpNoRefinement != underlying
    && dotc.core.Symbols.defn.isNonRefinedFunction(tpNoRefinement)
  def isTupleN: Boolean =
    dotc.core.Symbols.defn.isTupleNType(underlying)
  def select(sym: pub.Symbol): TypeRepr = TypeReprImpl(underlying.select(sym.cast.symbol))
  def appliedTo(targ: pub.TypeRepr): TypeRepr =
    TypeReprImpl(dotc.core.Types.decorateTypeApplications(underlying).appliedTo(targ.cast.underlying))
  def appliedTo(targs: List[pub.TypeRepr]): TypeRepr =
    TypeReprImpl(dotc.core.Types.decorateTypeApplications(underlying).appliedTo(targs.map(_.cast.underlying)))
  def substituteTypes(from: List[pub.Symbol], to: List[pub.TypeRepr]): TypeRepr =
    TypeReprImpl(underlying.subst(from.map(_.cast.symbol), to.map(_.cast.underlying)))

  def typeArgs: List[TypeRepr] = underlying match
    case dotc.core.Types.AppliedType(_, args)     => args.map(TypeReprImpl(_))
    case dotc.core.Types.AnnotatedType(parent, _) => TypeReprImpl(parent).typeArgs
    case dotc.core.Types.FlexibleType(underlying) => TypeReprImpl(underlying).typeArgs
    case _                                        => List.empty

}
object TypeReprImpl {

  def apply(t: dotc.core.Types.Type)(using Context): TypeReprImpl = t match {
    case t: dotc.core.Types.TermRef                                                             => new TermRefImpl(t)
    case t: dotc.core.Types.TypeRef                                                             => new TypeRefImpl(t)
    case t: dotc.core.Types.ConstantType                                                        => new ConstantTypeImpl(t)
    case t: dotc.core.Types.SuperType                                                           => new SuperTypeImpl(t)
    case t: dotc.core.Types.RefinedType                                                         => new RefinementImpl(t)
    case t: dotc.core.Types.AppliedType if t.tycon.isRef(dotc.core.Symbols.defn.MatchCaseClass) => new MatchCaseImpl(t)
    case t: dotc.core.Types.AppliedType                                                         => new AppliedTypeImpl(t)
    case t: dotc.core.Types.AnnotatedType                                                       => new AnnotatedTypeImpl(t)
    case t: dotc.core.Types.AndType                                                             => new AndTypeImpl(t)
    case t: dotc.core.Types.OrType                                                              => new OrTypeImpl(t)
    case t: dotc.core.Types.MatchType                                                           => new MatchTypeImpl(t)
    case t: dotc.core.Types.ExprType                                                            => new ByNameTypeImpl(t)
    case t: dotc.core.Types.ParamRef                                                            => new ParamRefImpl(t)
    case t: dotc.core.Types.ThisType                                                            => new ThisTypeImpl(t)
    case t: dotc.core.Types.RecThis                                                             => new RecursiveThisImpl(t)
    case t: dotc.core.Types.RecType                                                             => new RecursiveTypeImpl(t)
    case t: dotc.core.Types.MethodType                                                          => new MethodTypeImpl(t)
    case t: dotc.core.Types.PolyType                                                            => new PolyTypeImpl(t)
    case t: dotc.core.Types.HKTypeLambda                                                        => new TypeLambdaImpl(t)
    case t: dotc.core.Types.TypeBounds                                                          => new TypeBoundsImpl(t)
    case t: dotc.core.Types.NoPrefix.type                                                       => new NoPrefixImpl(t)
    case t: dotc.core.Types.FlexibleType                                                        => new FlexibleTypeImpl(t)
    case _                                                                                      => throw new MatchError(t)
  }

  object Module extends pub.TypeRepr.Module {
    override def of[T <: AnyKind](using scala.quoted.Type[T]): pub.TypeRepr = ???
    override def typeConstructorOf(clazz: Class[?]): pub.TypeRepr = ???
  }
}

/////// NamedType ///////////////////////////////////////////////////////////////

type NamedType = pub.NamedType & NamedTypeImpl
sealed trait NamedTypeImpl(using Context) extends TypeReprImpl { _self: pub.NamedType =>

  override val underlying: dotc.core.Types.NamedType

  def qualifier: TypeRepr = TypeReprImpl(underlying.prefix.widenSkolem)
  def name: String = underlying.name.toString

}
object NamedTypeImpl {
  object Module extends pub.NamedType.Module {}
}

/////// TermRef ///////////////////////////////////////////////////////////////

type TermRef = TermRefImpl // TODO (KR) : QuotesImpl.scala used only `NamedType` here, was that a mistake or intentional...
final class TermRefImpl(val underlying: dotc.core.Types.TermRef)(using Context) extends NamedTypeImpl, pub.TermRef
object TermRefImpl {
  object Module extends pub.TermRef.Module {
    override def apply(qual: pub.TypeRepr, name: String): pub.TermRef = ???
    override def make(qual: pub.TypeRepr, name: String): pub.TermRef = ???
  }
}

/////// TypeRef ///////////////////////////////////////////////////////////////

type TypeRef = TypeRefImpl
final class TypeRefImpl(val underlying: dotc.core.Types.TypeRef)(using Context) extends NamedTypeImpl, pub.TypeRef {
  def isOpaqueAlias: Boolean = underlying.symbol.isOpaqueAlias
  def translucentSuperType: TypeRepr = TypeReprImpl(underlying.translucentSuperType)
}
object TypeRefImpl {
  object Module extends pub.TypeRef.Module {}
}

/////// ConstantType ///////////////////////////////////////////////////////////////

type ConstantType = ConstantTypeImpl
final class ConstantTypeImpl(val underlying: dotc.core.Types.ConstantType)(using Context) extends TypeReprImpl, pub.ConstantType {
  override def constant: ConstantImpl = ConstantImpl(underlying.value)
}
object ConstantTypeImpl {
  object Module extends pub.ConstantType.Module {
    override def apply(x: pub.Constant): pub.ConstantType = ???
    override def make(x: pub.Constant): pub.ConstantType = ???
  }
}

/////// SuperType ///////////////////////////////////////////////////////////////

type SuperType = SuperTypeImpl
final class SuperTypeImpl(val underlying: dotc.core.Types.SuperType)(using Context) extends TypeReprImpl, pub.SuperType {
  override def thistpe: TypeRepr = TypeReprImpl(underlying.thistpe)
  override def supertpe: TypeRepr = TypeReprImpl(underlying.supertpe)
}
object SuperTypeImpl {
  object Module extends pub.SuperType.Module {
    override def apply(thistpe: pub.TypeRepr, supertpe: pub.TypeRepr): pub.SuperType = ???
    override def make(thistpe: pub.TypeRepr, supertpe: pub.TypeRepr): pub.SuperType = ???
  }
}

/////// Refinement ///////////////////////////////////////////////////////////////

type Refinement = RefinementImpl
final class RefinementImpl(val underlying: dotc.core.Types.RefinedType)(using Context) extends TypeReprImpl, pub.Refinement {
  override def parent: TypeRepr = TypeReprImpl(underlying.parent)
  override def name: String = underlying.refinedName.toString
  override def info: TypeRepr = TypeReprImpl(underlying.refinedInfo)
}
object RefinementImpl {
  object Module extends pub.Refinement.Module {
    override def apply(parent: pub.TypeRepr, name: String, info: pub.TypeRepr): pub.Refinement = ???
    override def make(parent: pub.TypeRepr, name: String, info: pub.TypeRepr): pub.Refinement = ???
  }
}

/////// AppliedType ///////////////////////////////////////////////////////////////

type AppliedType = AppliedTypeImpl
final class AppliedTypeImpl(val underlying: dotc.core.Types.AppliedType)(using Context) extends TypeReprImpl, pub.AppliedType {
  override def tycon: TypeRepr = TypeReprImpl(underlying.tycon)
  override def args: List[TypeRepr] = underlying.args.map(TypeReprImpl(_))
}
object AppliedTypeImpl {
  object Module extends pub.AppliedType.Module {
    override def apply(tycon: pub.TypeRepr, args: List[pub.TypeRepr]): pub.AppliedType = ???
    override def make(tycon: pub.TypeRepr, args: List[pub.TypeRepr]): pub.AppliedType = ???
  }
}

/////// AnnotatedType ///////////////////////////////////////////////////////////////

type AnnotatedType = AnnotatedTypeImpl
final class AnnotatedTypeImpl(val underlying: dotc.core.Types.AnnotatedType)(using Context) extends TypeReprImpl, pub.AnnotatedType {
  override def underlying: TypeRepr = TypeReprImpl(underlying.underlying)
}
object AnnotatedTypeImpl {
  object Module extends pub.AnnotatedType.Module {}
}

/////// AndOrType ///////////////////////////////////////////////////////////////

type AndOrType = pub.AndOrType & AndOrTypeImpl
sealed trait AndOrTypeImpl(using Context) extends TypeReprImpl { _self: pub.AndOrType =>
  override val underlying: dotc.core.Types.AndOrType
  def left: TypeRepr = TypeReprImpl(underlying.tp1)
  def right: TypeRepr = TypeReprImpl(underlying.tp2)
}
object AndOrTypeImpl {
  object Module extends pub.AndOrType.Module {}
}

/////// AndType ///////////////////////////////////////////////////////////////

type AndType = AndTypeImpl
final class AndTypeImpl(val underlying: dotc.core.Types.AndType)(using Context) extends AndOrTypeImpl, pub.AndType
object AndTypeImpl {
  object Module extends pub.AndType.Module {
    override def apply(lhs: pub.TypeRepr, rhs: pub.TypeRepr): pub.AndType = ???
    override def make(lhs: pub.TypeRepr, rhs: pub.TypeRepr): pub.AndType = ???
  }
}

/////// OrType ///////////////////////////////////////////////////////////////

type OrType = OrTypeImpl
final class OrTypeImpl(val underlying: dotc.core.Types.OrType)(using Context) extends AndOrTypeImpl, pub.OrType
object OrTypeImpl {
  object Module extends pub.OrType.Module {
    override def apply(lhs: pub.TypeRepr, rhs: pub.TypeRepr): pub.OrType = ???
    override def make(lhs: pub.TypeRepr, rhs: pub.TypeRepr): pub.OrType = ???
  }
}

/////// MatchType ///////////////////////////////////////////////////////////////

type MatchType = MatchTypeImpl
final class MatchTypeImpl(val underlying: dotc.core.Types.MatchType)(using Context) extends TypeReprImpl, pub.MatchType {
  override def bound: TypeRepr = TypeReprImpl(underlying.bound)
  override def scrutinee: TypeRepr = TypeReprImpl(underlying.scrutinee)
  override def cases: List[TypeRepr] = underlying.cases.map(TypeReprImpl(_))
}
object MatchTypeImpl {
  object Module extends pub.MatchType.Module {
    override def apply(bound: pub.TypeRepr, scrutinee: pub.TypeRepr, cases: List[pub.TypeRepr]): pub.MatchType = ???
    override def make(bound: pub.TypeRepr, scrutinee: pub.TypeRepr, cases: List[pub.TypeRepr]): pub.MatchType = ???
  }
}

/////// ByNameType ///////////////////////////////////////////////////////////////

type ByNameType = ByNameTypeImpl
final class ByNameTypeImpl(val underlying: dotc.core.Types.ExprType)(using Context) extends TypeReprImpl, pub.ByNameType {
  override def underlying: TypeRepr = TypeReprImpl(underlying.resType)
}
object ByNameTypeImpl {
  object Module extends pub.ByNameType.Module {
    override def apply(underlying: pub.TypeRepr): pub.TypeRepr = ???
    override def make(underlying: pub.TypeRepr): pub.TypeRepr = ???
  }
}

/////// ParamRef ///////////////////////////////////////////////////////////////

type ParamRef = ParamRefImpl
final class ParamRefImpl(val underlying: dotc.core.Types.ParamRef)(using Context) extends TypeReprImpl, pub.ParamRef {
  override def binder: TypeRepr = TypeReprImpl(underlying.binder)
  override def paramNum: Int = underlying.paramNum
}
object ParamRefImpl {
  object Module extends pub.ParamRef.Module {}
}

/////// ThisType ///////////////////////////////////////////////////////////////

type ThisType = ThisTypeImpl
final class ThisTypeImpl(val underlying: dotc.core.Types.ThisType)(using Context) extends TypeReprImpl, pub.ThisType {
  override def tref: TypeRepr = TypeReprImpl(underlying.tref)
}
object ThisTypeImpl {
  object Module extends pub.ThisType.Module {}
}

/////// RecursiveThis ///////////////////////////////////////////////////////////////

type RecursiveThis = RecursiveThisImpl
final class RecursiveThisImpl(val underlying: dotc.core.Types.RecThis)(using Context) extends TypeReprImpl, pub.RecursiveThis {
  override def binder: RecursiveType = RecursiveTypeImpl(underlying.binder)
}
object RecursiveThisImpl {
  object Module extends pub.RecursiveThis.Module {}
}

/////// RecursiveType ///////////////////////////////////////////////////////////////

type RecursiveType = RecursiveTypeImpl
final class RecursiveTypeImpl(val underlying: dotc.core.Types.RecType)(using Context) extends TypeReprImpl, pub.RecursiveType {
  override def underlying: TypeRepr = TypeReprImpl(underlying.underlying)
  override def recThis: RecursiveThis = RecursiveThisImpl(underlying.recThis)
}
object RecursiveTypeImpl {
  object Module extends pub.RecursiveType.Module {
    override def apply(parentExp: pub.RecursiveType => pub.TypeRepr): pub.RecursiveType = ???
    override def make(parentExp: pub.RecursiveType => pub.TypeRepr): pub.RecursiveType = ???
  }
}

/////// LambdaType ///////////////////////////////////////////////////////////////

type LambdaType = pub.LambdaType & LambdaTypeImpl
sealed trait LambdaTypeImpl(using Context) extends TypeReprImpl { _self: pub.LambdaType =>
  override val underlying: dotc.core.Types.LambdaType
  def paramNames: List[String] = underlying.paramNames.map(_.toString)
  def paramTypes: List[TypeRepr] = underlying.paramInfos.map(TypeReprImpl(_))
  def resType: TypeRepr = TypeReprImpl(underlying.resType)
}
object LambdaTypeImpl {
  object Module extends pub.LambdaType.Module {}
}

/////// MethodOrPoly ///////////////////////////////////////////////////////////////

type MethodOrPoly = pub.MethodOrPoly & MethodOrPolyImpl
sealed trait MethodOrPolyImpl(using Context) extends LambdaTypeImpl { _self: pub.MethodOrPoly => }
object MethodOrPolyImpl {
  object Module extends pub.MethodOrPoly.Module {}
}

/////// MethodType ///////////////////////////////////////////////////////////////

type MethodType = MethodTypeImpl
final class MethodTypeImpl(val underlying: dotc.core.Types.MethodType)(using Context) extends MethodOrPolyImpl, pub.MethodType {
  override def isImplicit: Boolean = underlying.isImplicitMethod
  override def isContextual: Boolean = underlying.isContextualMethod
  override def methodTypeKind: pub.MethodTypeKind =
    underlying.companion match
      case dotc.core.Types.ContextualMethodType => pub.MethodTypeKind.Contextual
      case dotc.core.Types.ImplicitMethodType   => pub.MethodTypeKind.Implicit
      case _                                    => pub.MethodTypeKind.Plain

  override def erasedParams: List[Boolean] = underlying.paramErasureStatuses
  override def hasErasedParams: Boolean = underlying.hasErasedParams
  override def param(idx: Int): TypeRepr = TypeReprImpl(underlying.newParamRef(idx))
}
object MethodTypeImpl {
  object Module extends pub.MethodType.Module {
    override def apply(paramNames: List[String])(paramInfosExp: pub.MethodType => List[pub.TypeRepr], resultTypeExp: pub.MethodType => pub.TypeRepr): pub.MethodType = ???
    override def make(paramNames: List[String])(paramInfosExp: pub.MethodType => List[pub.TypeRepr], resultTypeExp: pub.MethodType => pub.TypeRepr): pub.MethodType = ???
    override def apply(kind: pub.MethodTypeKind)(paramNames: List[String])(paramInfosExp: pub.MethodType => List[pub.TypeRepr], resultTypeExp: pub.MethodType => pub.TypeRepr): pub.MethodType = ???
    override def make(kind: pub.MethodTypeKind)(paramNames: List[String])(paramInfosExp: pub.MethodType => List[pub.TypeRepr], resultTypeExp: pub.MethodType => pub.TypeRepr): pub.MethodType = ???
  }
}

/////// PolyType ///////////////////////////////////////////////////////////////

type PolyType = PolyTypeImpl
final class PolyTypeImpl(val underlying: dotc.core.Types.PolyType)(using Context) extends MethodOrPolyImpl, pub.PolyType {
  override def param(idx: Int): TypeRepr = TypeReprImpl(underlying.newParamRef(idx))
  override def paramBounds: List[TypeBounds] = underlying.paramInfos.map(TypeBoundsImpl(_))
}
object PolyTypeImpl {
  object Module extends pub.PolyType.Module {
    override def apply(paramNames: List[String])(paramBoundsExp: pub.PolyType => List[pub.TypeBounds], resultTypeExp: pub.PolyType => pub.TypeRepr): pub.PolyType = ???
    override def make(paramNames: List[String])(paramBoundsExp: pub.PolyType => List[pub.TypeBounds], resultTypeExp: pub.PolyType => pub.TypeRepr): pub.PolyType = ???
  }
}

/////// TypeLambda ///////////////////////////////////////////////////////////////

type TypeLambda = TypeLambdaImpl
final class TypeLambdaImpl(val underlying: dotc.core.Types.HKTypeLambda)(using Context) extends LambdaTypeImpl, pub.TypeLambda {
  override def param(idx: Int): TypeRepr = TypeReprImpl(underlying.newParamRef(idx))
  override def paramBounds: List[TypeBounds] = underlying.paramInfos.map(TypeBoundsImpl(_))
  override def paramVariances: List[pub.Flags] = underlying.typeParams.map(tp => new FlagsImpl(tp.paramVariance))
}
object TypeLambdaImpl {
  object Module extends pub.TypeLambda.Module {
    override def apply(paramNames: List[String], boundsFn: pub.TypeLambda => List[pub.TypeBounds], bodyFn: pub.TypeLambda => pub.TypeRepr): pub.TypeLambda = ???
    override def make(paramNames: List[String], boundsFn: pub.TypeLambda => List[pub.TypeBounds], bodyFn: pub.TypeLambda => pub.TypeRepr): pub.TypeLambda = ???
  }
}

/////// MatchCase ///////////////////////////////////////////////////////////////

type MatchCase = MatchCaseImpl
final class MatchCaseImpl(val underlying: dotc.core.Types.AppliedType)(using Context) extends TypeReprImpl, pub.MatchCase {
  override def pattern: TypeRepr = TypeReprImpl(underlying.args(0))
  override def rhs: TypeRepr = TypeReprImpl(underlying.args(1))
}
object MatchCaseImpl {
  object Module extends pub.MatchCase.Module {
    override def apply(pattern: pub.TypeRepr, rhs: pub.TypeRepr): pub.MatchCase = ???
    override def make(pattern: pub.TypeRepr, rhs: pub.TypeRepr): pub.MatchCase = ???
  }
}

/////// TypeBounds ///////////////////////////////////////////////////////////////

type TypeBounds = TypeBoundsImpl
final class TypeBoundsImpl(val underlying: dotc.core.Types.TypeBounds)(using Context) extends TypeReprImpl, pub.TypeBounds {
  override def low: TypeRepr = TypeReprImpl(underlying.lo)
  override def hi: TypeRepr = TypeReprImpl(underlying.hi)
}
object TypeBoundsImpl {
  object Module extends pub.TypeBounds.Module {
    override def apply(low: pub.TypeRepr, hi: pub.TypeRepr): pub.TypeBounds = ???
    override def make(low: pub.TypeRepr, hi: pub.TypeRepr): pub.TypeBounds = ???
    override def empty: pub.TypeBounds = ???
    override def upper(hi: pub.TypeRepr): pub.TypeBounds = ???
    override def lower(lo: pub.TypeRepr): pub.TypeBounds = ???
  }
}

/////// NoPrefix ///////////////////////////////////////////////////////////////

type NoPrefix = NoPrefixImpl
final class NoPrefixImpl(val underlying: dotc.core.Types.NoPrefix.type)(using Context) extends TypeReprImpl, pub.NoPrefix
object NoPrefixImpl {
  object Module extends pub.NoPrefix.Module {}
}

/////// FlexibleType ///////////////////////////////////////////////////////////////

type FlexibleType = FlexibleTypeImpl
final class FlexibleTypeImpl(val underlying: dotc.core.Types.FlexibleType)(using Context) extends TypeReprImpl, pub.FlexibleType {
  override def underlying: TypeRepr = TypeReprImpl(underlying.hi)
  override def lo: TypeRepr = TypeReprImpl(underlying.lo)
  override def hi: TypeRepr = TypeReprImpl(underlying.hi)
}
object FlexibleTypeImpl {
  object Module extends pub.FlexibleType.Module {
    override def apply(tp: pub.TypeRepr): pub.FlexibleType = ???
    override def make(tp: pub.TypeRepr): pub.FlexibleType = ???
  }
}