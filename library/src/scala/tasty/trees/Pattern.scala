package scala.tasty
package trees

import scala.runtime.tasty.Toolbox
import scala.tasty.types.Type

trait Pattern extends Tree {
  def tpe: Type
}

object Value {
  type Data = Term
  def unapply(arg: Pattern)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyValue(arg)
}

object Bind {
  type Data = (names.TermName, Pattern)
  def unapply(arg: Pattern)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyBind(arg)
}

object Unapply {
  type Data = (Term, List[Term], List[Pattern])
  def unapply(arg: Pattern)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyUnapply(arg)
}

object Alternative {
  type Data = List[Pattern]
  def unapply(arg: Pattern)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyAlternative(arg)
}

object TypeTest {
  type Data = TypeTree
  def unapply(arg: Pattern)(implicit toolbox: Toolbox): Option[Data] = toolbox.unapplyTypeTest(arg)
}
