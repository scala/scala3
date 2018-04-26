package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types
import dotty.tools.dotc.core.Names

import scala.tasty.types

object Type {

  def apply(arg: Types.Type): types.Type = arg match {
    case arg: Types.LambdaType => LambdaType(arg)
    case _ => new Impl(arg)
  }

  def unapplyConstantType(arg: Impl)(implicit ctx: Context): Option[types.ConstantType.Data] = arg.tpe match {
    case Types.ConstantType(value) => Some(Constant(value))
    case _ => None
  }

  def unapplySymRef(arg: Impl)(implicit ctx: Context): Option[types.SymRef.Data] = arg.tpe match {
    case tp: Types.NamedType =>
      tp.designator match {
        case sym: Symbol => Some(Definition(sym), TypeOrNoPrefix(tp.prefix))
        case _ => None
      }
    case _ => None
  }

  def unapplyNameRef(arg: Impl)(implicit ctx: Context): Option[types.NameRef.Data] = arg.tpe match {
    case tp: Types.NamedType =>
      tp.designator match {
        case name: Names.Name => Some(Name(name), TypeOrNoPrefix(tp.prefix))
        case _ => None
      }
    case _ => None
  }

  def unapplySuperType(arg: Impl)(implicit ctx: Context): Option[types.SuperType.Data] = arg.tpe match {
    case Types.SuperType(thistpe, supertpe) => Some(Type(thistpe), Type(supertpe))
    case _ => None
  }

  def unapplyRefinement(arg: Impl)(implicit ctx: Context): Option[types.Refinement.Data] = arg.tpe match {
    case Types.RefinedType(parent, name, info) =>
      Some((Type(parent), if (name.isTermName) TermName(name.asTermName) else TypeName(name.asTypeName), MaybeType(info)))
    case _ => None
  }

  def unapplyAppliedType(arg: Impl)(implicit ctx: Context): Option[types.AppliedType.Data] = arg.tpe match {
    case Types.AppliedType(tycon, args) =>
      Some((Type(tycon), args.map { case arg: Types.TypeBounds => TypeBounds(arg); case arg => Type(arg) }))
    case _ => None
  }

  def unapplyAnnotatedType(arg: Impl)(implicit ctx: Context): Option[types.AnnotatedType.Data] = arg.tpe match {
    case Types.AnnotatedType(underlying, annot) => Some((Type(underlying), Term(annot.tree)))
    case _ => None
  }

  def unapplyAndType(arg: Impl)(implicit ctx: Context): Option[types.AndType.Data] = arg.tpe match {
    case Types.AndType(left, right) => Some(Type(left), Type(right))
    case _ => None
  }

  def unapplyOrType(arg: Impl)(implicit ctx: Context): Option[types.OrType.Data] = arg.tpe match {
    case Types.OrType(left, right) => Some(Type(left), Type(right))
    case _ => None
  }

  def unapplyByNameType(arg: Impl)(implicit ctx: Context): Option[types.ByNameType.Data] = arg.tpe match {
    case Types.ExprType(resType) => Some(Type(resType))
    case _ => None
  }

  def unapplyParamRef(arg: Impl)(implicit ctx: Context): Option[types.ParamRef.Data] = arg.tpe match {
    case Types.TypeParamRef(binder, idx) => Some(TypeLambda(binder), idx)
    case _ => None
  }

  def unapplyThisType(arg: Impl)(implicit ctx: Context): Option[types.ThisType.Data] = arg.tpe match {
    case Types.ThisType(tp) => Some(Type(tp))
    case _ => None
  }

  def unapplyRecursiveThis(arg: Impl)(implicit ctx: Context): Option[types.RecursiveThis.Data] = arg.tpe match {
    case Types.RecThis(binder) => Some(RecursiveType(binder))
    case _ => None
  }

  private[tasty] class Impl(val tpe: Types.Type) extends types.Type {

    assert(!tpe.isInstanceOf[Types.TypeBounds])

    override def toString: String = "Type"
  }

}
