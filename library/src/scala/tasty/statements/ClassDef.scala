package scala.tasty
package statements

import scala.runtime.tasty.Toolbox
import scala.tasty.modifiers.Modifier

trait ClassDef extends Definition

object ClassDef {
  // TODO Replace when bootstrapped
  // List[scala.util.Either[terms.Term, typetrees.TypeTree] is used where we should have Term | Type in dotty.
  type Data = (names.TypeName, DefDef, List[scala.util.Either[terms.Term, typetrees.TypeTree]],  Option[ValDef], List[Statement], List[Modifier])
  def unapply(arg: TopLevelStatement)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyClassDef(arg)
}
