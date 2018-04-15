package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.core.Names

import scala.tasty.types

object Type {

  def apply(arg: Types.Type)(implicit ctx: Context): types.Type = arg match {
    case arg: Types.LambdaType => LambdaType(arg)
    case _ => Impl(arg, ctx)
  }

  def unapplyConstantType(arg: types.MaybeType): Option[types.ConstantType.Data] = arg match {
    case Impl(Types.ConstantType(value), _) => Some(Constant(value))
    case _ => None
  }

  def unapplySymRef(arg: types.MaybeType): Option[types.SymRef.Data] = arg match {
    case Impl(tp: Types.NamedType, ctx) =>
      implicit val ctx_ = ctx
      tp.designator match {
        case sym: Symbol => Some(TastySymbol(sym), TypeOrNoPrefix(tp.prefix))
        case _ => None
      }
    case _ => None
  }

  def unapplyNameRef(arg: types.MaybeType): Option[types.NameRef.Data] = arg match {
    case Impl(tp: Types.NamedType, ctx) =>
      implicit val ctx_ = ctx
      tp.designator match {
        case name: Names.Name => Some(Name(name), TypeOrNoPrefix(tp.prefix))
        case _ => None
      }
    case _ => None
  }

  def unapplySuperType(arg: types.MaybeType): Option[types.SuperType.Data] = arg match {
    case Impl(Types.SuperType(thistpe, supertpe), ctx) => Some(Type(thistpe)(ctx), Type(supertpe)(ctx))
    case _ => None
  }

  def unapplyRefinement(arg: types.MaybeType): Option[types.Refinement.Data] = arg match {
    case Impl(Types.RefinedType(parent, name, info), ctx) =>
      Some((Type(parent)(ctx), if (name.isTermName) TermName(name.asTermName) else TypeName(name.asTypeName), MaybeType(info)(ctx)))
    case _ => None
  }

  def unapplyAppliedType(arg: types.MaybeType): Option[types.AppliedType.Data] = arg match {
    case Impl(Types.AppliedType(tycon, args), ctx) =>
      Some((Type(tycon)(ctx), args.map { case arg: Types.TypeBounds => TypeBounds(arg)(ctx); case arg => Type(arg)(ctx) }))
    case _ => None
  }

  def unapplyAnnotatedType(arg: types.MaybeType): Option[types.AnnotatedType.Data] = arg match {
    case Impl(Types.AnnotatedType(underlying, annot), ctx) => Some((Type(underlying)(ctx), Term(annot.tree(ctx))(ctx)))
    case _ => None
  }

  def unapplyAndType(arg: types.MaybeType): Option[types.AndType.Data] = arg match {
    case Impl(Types.AndType(left, right), ctx) => Some(Type(left)(ctx), Type(right)(ctx))
    case _ => None
  }

  def unapplyOrType(arg: types.MaybeType): Option[types.OrType.Data] = arg match {
    case Impl(Types.OrType(left, right), ctx) => Some(Type(left)(ctx), Type(right)(ctx))
    case _ => None
  }

  def unapplyByNameType(arg: types.MaybeType): Option[types.ByNameType.Data] = arg match {
    case Impl(Types.ExprType(resType), ctx) => Some(Type(resType)(ctx))
    case _ => None
  }

  def unapplyParamRef(arg: types.MaybeType): Option[types.ParamRef.Data] = arg match {
    case Impl(Types.TypeParamRef(binder, idx), ctx) => Some(TypeLambda(binder)(ctx), idx)
    case _ => None
  }

  def unapplyThisType(arg: types.MaybeType): Option[types.ThisType.Data] = arg match {
    case Impl(Types.ThisType(tp), ctx) => Some(Type(tp)(ctx))
    case _ => None
  }

  def unapplyRecursiveThis(arg: types.MaybeType): Option[types.RecursiveThis.Data] = arg match {
    case Impl(Types.RecThis(binder), ctx) => Some(RecursiveType(binder)(ctx))
    case _ => None
  }

  private case class Impl(arg: Types.Type, ctx: Context) extends types.Type {

    assert(!arg.isInstanceOf[Types.TypeBounds])

    override def toString: String = {
      import Toolbox.extractor
      this match {
        case types.ConstantType(value) => s"ConstantType($value)"
        case types.SymRef(sym, qual) => s"SymRef($sym, $qual)"
        case types.NameRef(name, qual) => s"NameRef($name, $qual)"
        case types.Refinement(parent, name, info) => s"Refinement($parent, $name, $info)"
        case types.AppliedType(tycon, args) => s"AppliedType($tycon, $args)"
        case types.AnnotatedType(underlying, annot) => s"AnnotatedType($underlying, $annot)"
        case types.AndType(left, right) => s"AndType($left, $right)"
        case types.OrType(left, right) => s"OrType($left, $right)"
        case types.ByNameType(underlying) => s"ByNameType($underlying)"
        case types.ParamRef(binder, idx) => s"ParamRef($binder, $idx)"
        case types.ThisType(tp) => s"ThisType($tp)"
        case types.RecursiveThis(binder) => s"RecursiveThis($binder)"
      }
    }
  }

}
