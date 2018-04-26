package scala.tasty
package trees

import scala.tasty.modifiers.Modifier

trait ClassDef extends Definition {
  def mods(implicit ctx: Context): List[Modifier]
}

object ClassDef {
  type Data = (names.TypeName, DefDef, List[Tree] /* List[Term | TypeTree] */,  Option[ValDef], List[Statement])
  def unapply(arg: ClassDef)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyClassDef(arg)
}
