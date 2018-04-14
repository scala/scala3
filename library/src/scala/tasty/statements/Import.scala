package scala.tasty
package statements

import scala.runtime.tasty.Toolbox

trait Import extends Statement

object Import {
  type Data = (terms.Term, List[ImportSelector])
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyImport(arg)

  sealed trait ImportSelector
  object ImportSelector {
    final case class Simple(id: Id) extends ImportSelector
    final case class Rename(id1: Id, id2: Id) extends ImportSelector
    final case class Omit(id: Id) extends ImportSelector
  }
}

