package scala.tasty
package types

trait LambdaType[ParamName <: names.Name, ParamInfo <: MaybeType] extends Type

trait MethodType extends LambdaType[names.TermName, Type] {
  def isImplicit: Boolean
  def isErased: Boolean
}
object MethodType {
  type Data = (List[names.TermName], List[Type], Type)
  def unapply(arg: MethodType)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyMethodType(arg)
}

trait PolyType extends LambdaType[names.TypeName, TypeBounds]
object PolyType {
  type Data = (List[names.TypeName], List[TypeBounds], Type)
  def unapply(arg: PolyType)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyPolyType(arg)
}

trait TypeLambda extends LambdaType[names.TypeName, TypeBounds]
object TypeLambda {
  type Data = (List[names.TypeName], List[TypeBounds], Type)
  def unapply(arg: TypeLambda)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyTypeLambda(arg)
}
