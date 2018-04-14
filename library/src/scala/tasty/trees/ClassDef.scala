package scala.tasty.trees

import scala.runtime.tasty.Toolbox
import scala.tasty.modifiers.Modifier
import scala.tasty.{Tree, names}

trait ClassDef extends Definition

object ClassDef {

  type Data = (names.TypeName, DefDef, List[Tree] /* List[Term | TypeTree] */,  Option[ValDef], List[Statement], List[Modifier])
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyClassDef(arg)
}
