package scala.tasty
package trees

import scala.runtime.tasty.Toolbox

trait PackageDef extends Definition

object PackageDef {
  type Data = (names.Name, List[Statement])
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyPackageDef(arg)
}
