package scala.tasty
package trees

import scala.tasty.modifiers.Modifier

trait ValDef extends Definition

object ValDef {
  type Data = (names.TermName, TypeTree, Option[Term], List[Modifier])
  def unapply(arg: ValDef)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyValDef(arg)
}
