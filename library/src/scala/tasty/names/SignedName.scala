package scala.tasty.names

import scala.tasty.Extractor

trait SignedName extends PossiblySignedName

object SignedName {
  type Data = (TermName, TypeName, List[TypeName])
  def unapply(arg: PossiblySignedName)(implicit ext: Extractor): Option[Data] = ext.unapplySignedName(arg)
}
