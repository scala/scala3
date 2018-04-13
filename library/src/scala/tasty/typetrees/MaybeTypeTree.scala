package scala.tasty.typetrees

import scala.tasty.Positioned
import scala.tasty.types.MaybeType

trait MaybeTypeTree extends Positioned {
  def tpe: MaybeType
}
