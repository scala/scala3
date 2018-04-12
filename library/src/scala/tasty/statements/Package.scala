package scala.tasty.statements

import scala.runtime.tasty.Toolbox
import scala.tasty.terms.Term

trait Package extends TopLevelStatement

object Package {
  type Data = (Term, List[TopLevelStatement])
  def unapply(arg: TopLevelStatement)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyPackage(arg)
}
