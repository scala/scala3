package scala.tasty
package statements

import scala.runtime.tasty.Toolbox
import scala.tasty.terms.Term

trait Package extends TopLevelStatement

object Package {
  type Data = (Term, List[TopLevelStatement])
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyPackage(arg)
}
