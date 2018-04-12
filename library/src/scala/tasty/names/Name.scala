package scala.tasty.names

trait Name

trait PossiblySignedName

trait TermName extends Name with PossiblySignedName

trait SignedName extends PossiblySignedName

trait TypeName extends Name
