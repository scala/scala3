package scala.tasty
package trees

import scala.runtime.tasty.Toolbox

trait Import extends Statement

object Import {
  type Data = (Term, List[ImportSelector])
  def unapply(arg: Import)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyImport(arg)
}
