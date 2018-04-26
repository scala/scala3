package scala.tasty
package trees

import scala.tasty.modifiers.Modifier

trait DefDef extends Definition {
  def mods(implicit ctx: Context): List[Modifier]
}

object DefDef {
  type Data = (names.TermName, List[TypeDef],  List[List[ValDef]], TypeTree, Option[Term])
  def unapply(arg: DefDef)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyDefDef(arg)
}
