package scala.tasty
package trees

trait PackageDef extends Definition

object PackageDef {
  type Data = (names.Name, List[Statement])
  def unapply(arg: PackageDef)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyPackageDef(arg)
}
