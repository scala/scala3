package scala.tasty.typetrees

import scala.tasty.Positioned
import scala.tasty.types.MaybeType

/** Trees denoting types */
trait TypeTree extends Positioned {
  def tpe: MaybeType
}
