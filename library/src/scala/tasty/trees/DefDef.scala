package scala.tasty.trees

import scala.runtime.tasty.Toolbox
import scala.tasty.modifiers.Modifier
import scala.tasty.{Tree, names}

trait DefDef extends Definition

object DefDef {
  type Data = (names.TermName, List[TypeDef],  List[List[ValDef]], TypeTree, Option[Term], List[Modifier])
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyDefDef(arg)
}
