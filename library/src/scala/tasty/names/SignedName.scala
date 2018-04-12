package scala.tasty.names

import scala.runtime.tasty.Toolbox

trait SignedName extends PossiblySignedName

object SignedName {
  type Data = (TermName, TypeName, List[TypeName])
  def unapply(arg: PossiblySignedName)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplySignedName(arg)
}
