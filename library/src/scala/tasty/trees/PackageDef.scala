package scala.tasty
package trees

import scala.runtime.tasty.Toolbox

trait PackageDef extends Definition

object PackageDef {
  type Data = (names.Name, List[Statement])
  def unapply(arg: PackageDef)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyPackageDef(arg)
}
