package scala.tasty.trees

import scala.runtime.tasty.Toolbox
import scala.tasty.Tree

trait PackageDef extends TopLevelStatement

object PackageDef {
  type Data = (Term, List[TopLevelStatement])
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyPackageDef(arg)
}
