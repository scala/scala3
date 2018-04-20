package scala.tasty
package trees

import scala.runtime.tasty.Toolbox

trait ImportSelector

object SimpleSelector {
  type Data = Id
  def unapply(arg: ImportSelector)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplySimpleSelector(arg)
}

object RenameSelector {
  type Data = (Id, Id)
  def unapply(arg: ImportSelector)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyRenameSelector(arg)
}

object OmitSelector {
  type Data = Id
  def unapply(arg: ImportSelector)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyOmitSelector(arg)
}
