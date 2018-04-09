package scala.tasty
package trees

trait PackageClause extends TopLevelStatement {
  def definition(implicit ctx: Context): Definition
}

object PackageClause {
  type Data = (Term, List[TopLevelStatement])
  def unapply(arg: PackageClause)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyPackageClause(arg)
}
