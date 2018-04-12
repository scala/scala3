package scala.tasty.types

import scala.runtime.tasty.Toolbox

trait TypeBounds extends MaybeType

object TypeBounds {
  type Data = (Type, Type)
  def unapply(arg: MaybeType)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyTypeBounds(arg)
}
