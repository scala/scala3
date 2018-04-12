package scala.tasty.names

import scala.tasty.Extractor

trait TypeName extends Name

object TypeName {
  type Data = TermName
  def unapply(arg: Name)(implicit ext: Extractor): Option[Data] = ext.unapplyTypeName(arg)
}
