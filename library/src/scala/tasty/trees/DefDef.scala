package scala.tasty
package trees

import scala.tasty.modifiers.Modifier

trait DefDef extends Definition

object DefDef {
  type Data = (names.TermName, List[TypeDef],  List[List[ValDef]], TypeTree, Option[Term], List[Modifier])
  def unapply(arg: DefDef)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyDefDef(arg)
}
