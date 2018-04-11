package scala.tasty

trait Name

trait PossiblySignedName

trait TermName extends Name with PossiblySignedName

trait SignedName extends PossiblySignedName

trait TypeName extends Name

object Simple {
  def unapply(arg: scala.tasty.Name)(implicit ext: Extractor): Option[String] = ext.unapplySimple(arg)
}