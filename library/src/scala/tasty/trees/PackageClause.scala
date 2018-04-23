package scala.tasty.trees

import scala.runtime.tasty.Toolbox

trait PackageClause extends TopLevelStatement

object PackageClause {
  type Data = (Term, List[TopLevelStatement])
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyPackageClause(arg)
}
