package scala.tasty
package trees

import scala.tasty.modifiers.Modifier

trait ValDef extends Definition {
  def mods(implicit ctx: Context): List[Modifier]
}

object ValDef {
  type Data = (names.TermName, TypeTree, Option[Term])
  def unapply(arg: ValDef)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyValDef(arg)
}
