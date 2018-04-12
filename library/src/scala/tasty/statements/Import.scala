package scala.tasty
package statements

import scala.tasty.Extractor

trait Import extends Statement

object Import {
  type Data = (terms.Term, List[ImportSelector])
  def unapply(arg: TopLevelStatement)(implicit ext: Extractor): Option[Data] = ext.unapplyImport(arg)

  sealed trait ImportSelector
  object ImportSelector {
    final case class Simple(id: Id) extends ImportSelector
    final case class Rename(id1: Id, id2: Id) extends ImportSelector
    final case class Omit(id: Id) extends ImportSelector
  }
}

