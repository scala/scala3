package scala.tasty.typetrees

import scala.tasty.types.Type

/** Trees denoting types */
trait TypeTree extends MaybeTypeTree {
  def tpe: Type
}
