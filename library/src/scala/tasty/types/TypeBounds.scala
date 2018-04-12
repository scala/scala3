package scala.tasty.types

import scala.tasty.Extractor

trait TypeBounds extends MaybeType

object TypeBounds {
  type Data = (Type, Type)
  def unapply(arg: MaybeType)(implicit ext: Extractor): Option[Data] = ext.unapplyTypeBounds(arg)
}
