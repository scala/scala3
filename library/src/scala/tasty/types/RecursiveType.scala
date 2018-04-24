package scala.tasty.types

import scala.runtime.tasty.Toolbox

trait RecursiveType extends Type

object RecursiveType {
  type Data = Type
  def unapply(arg: RecursiveType)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyRecursiveType(arg)
}
