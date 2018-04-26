package scala.tasty.trees

import scala.tasty.types

/** Trees denoting types */
trait TypeTree extends Tree {
  def tpe: types.Type
}
