package scala.tasty.typetrees

import scala.tasty.Positioned
import scala.tasty.types.Type

/** Trees denoting types */
trait TypeTree extends Positioned {
  def tpe: Type
}
