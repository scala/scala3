package scala.tasty.typetree

import scala.tasty.{Positioned, Type}

/** Trees denoting types */
trait TypeTree extends Positioned {
  def tpe: Type
}
