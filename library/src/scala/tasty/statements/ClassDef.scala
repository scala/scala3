package scala.tasty
package statements

import scala.runtime.tasty.Toolbox
import scala.tasty.modifiers.Modifier

trait ClassDef extends Definition

object ClassDef {
  type Data = (names.TypeName, DefDef, List[terms.Term],  Option[ValDef], List[Statement], List[Modifier])
  def unapply(arg: TopLevelStatement)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyClassDef(arg)
}
