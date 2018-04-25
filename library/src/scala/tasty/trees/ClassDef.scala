package scala.tasty
package trees

import scala.tasty.modifiers.Modifier

trait ClassDef extends Definition

object ClassDef {

  type Data = (names.TypeName, DefDef, List[Tree] /* List[Term | TypeTree] */,  Option[ValDef], List[Statement], List[Modifier])
  def unapply(arg: ClassDef)(implicit ctx: Context): Option[Data] = ctx.toolbox.unapplyClassDef(arg)
}
