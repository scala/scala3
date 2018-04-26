package scala.tasty
package types

trait TypeBounds extends MaybeType

object TypeBounds {
  type Data = (Type, Type)
  def unapply(arg: TypeBounds)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyTypeBounds(arg)
}
