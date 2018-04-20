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
    case _ => new Impl(arg)
  }

  def unapplyConstantType(arg: Impl): Option[types.ConstantType.Data] = arg.tpe match {
    case Types.ConstantType(value) => Some(Constant(value))
    case _ => None
  }

  def unapplySymRef(arg: Impl): Option[types.SymRef.Data] = arg.tpe match {
    case tp: Types.NamedType =>
      implicit val ctx: Context = arg.ctx
      tp.designator match {
        case sym: Symbol => Some(TastySymbol(sym), TypeOrNoPrefix(tp.prefix))
        case _ => None
      }
    case _ => None
  }

  def unapplyNameRef(arg: Impl): Option[types.NameRef.Data] = arg.tpe match {
    case tp: Types.NamedType =>
      implicit val ctx: Context = arg.ctx
      tp.designator match {
        case name: Names.Name => Some(Name(name), TypeOrNoPrefix(tp.prefix))
        case _ => None
      }
    case _ => None
  }

  def unapplySuperType(arg: Impl): Option[types.SuperType.Data] = arg.tpe match {
    case Types.SuperType(thistpe, supertpe) =>
      implicit val ctx: Context = arg.ctx
      Some(Type(thistpe), Type(supertpe))
    case _ => None
  }

  def unapplyRefinement(arg: Impl): Option[types.Refinement.Data] = arg.tpe match {
    case Types.RefinedType(parent, name, info) =>
      implicit val ctx: Context = arg.ctx
      Some((Type(parent), if (name.isTermName) TermName(name.asTermName) else TypeName(name.asTypeName), MaybeType(info)))
    case _ => None
  }

  def unapplyAppliedType(arg: Impl): Option[types.AppliedType.Data] = arg.tpe match {
    case Types.AppliedType(tycon, args) =>
      implicit val ctx: Context = arg.ctx
      Some((Type(tycon), args.map { case arg: Types.TypeBounds => TypeBounds(arg); case arg => Type(arg) }))
    case _ => None
  }

  def unapplyAnnotatedType(arg: Impl): Option[types.AnnotatedType.Data] = arg.tpe match {
    case Types.AnnotatedType(underlying, annot) =>
      implicit val ctx: Context = arg.ctx
      Some((Type(underlying), Term(annot.tree)))
    case _ => None
  }

  def unapplyAndType(arg: Impl): Option[types.AndType.Data] = arg.tpe match {
    case Types.AndType(left, right) =>
      implicit val ctx: Context = arg.ctx
      Some(Type(left), Type(right))
    case _ => None
  }

  def unapplyOrType(arg: Impl): Option[types.OrType.Data] = arg.tpe match {
    case Types.OrType(left, right) =>
      implicit val ctx: Context = arg.ctx
      Some(Type(left), Type(right))
    case _ => None
  }

  def unapplyByNameType(arg: Impl): Option[types.ByNameType.Data] = arg.tpe match {
    case Types.ExprType(resType) =>
      implicit val ctx: Context = arg.ctx
      Some(Type(resType))
    case _ => None
  }

  def unapplyParamRef(arg: Impl): Option[types.ParamRef.Data] = arg.tpe match {
    case Types.TypeParamRef(binder, idx) =>
      implicit val ctx: Context = arg.ctx
      Some(TypeLambda(binder), idx)
    case _ => None
  }

  def unapplyThisType(arg: Impl): Option[types.ThisType.Data] = arg.tpe match {
    case Types.ThisType(tp) =>
      implicit val ctx: Context = arg.ctx
      Some(Type(tp))
    case _ => None
  }

  def unapplyRecursiveThis(arg: Impl): Option[types.RecursiveThis.Data] = arg.tpe match {
    case Types.RecThis(binder) =>
      implicit val ctx: Context = arg.ctx
      Some(RecursiveType(binder))
    case _ => None
  }

  private[tasty] class Impl(val tpe: Types.Type)(implicit val ctx: Context) extends types.Type {

    assert(!tpe.isInstanceOf[Types.TypeBounds])

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
