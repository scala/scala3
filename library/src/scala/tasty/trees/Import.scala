package scala.tasty
package trees

trait Import extends Statement

object Import {
  type Data = (Term, List[ImportSelector])
  def unapply(arg: Import)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyImport(arg)
}
