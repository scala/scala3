package scala.tasty
package patterns

import scala.runtime.tasty.Toolbox
import scala.tasty.types.Type

trait Pattern extends Tree {
  def tpe: Type
}

object Value {
  type Data = terms.Term
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyValue(arg)
}

object Bind {
  type Data = (names.TermName, Pattern)
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyBind(arg)
}

object Unapply {
  type Data = (terms.Term, List[terms.Term], List[Pattern])
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyUnapply(arg)
}

object Alternative {
  type Data = List[Pattern]
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyAlternative(arg)
}

object TypeTest {
  type Data = typetrees.TypeTree
  def unapply(arg: Tree)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyTypeTest(arg)
}
