package scala.tasty.trees

import scala.runtime.tasty.Toolbox
import scala.tasty.modifiers.Modifier
import scala.tasty.names

trait ClassDef extends Definition

object ClassDef {

  type Data = (names.TypeName, DefDef, List[Tree] /* List[Term | TypeTree] */,  Option[ValDef], List[Statement], List[Modifier])
  def unapply(arg: ClassDef)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyClassDef(arg)
}
