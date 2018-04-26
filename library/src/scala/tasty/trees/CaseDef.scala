package scala.tasty
package trees

trait CaseDef extends Tree

object CaseDef {
  type Data = (Pattern, Option[Term], Term)
  def unapply(arg: CaseDef)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyCaseDef(arg)
}
