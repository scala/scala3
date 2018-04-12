package scala.tasty
package patterns

import scala.tasty.Extractor
import scala.tasty.types.Type

trait Pattern extends Positioned {
  def tpe: Type
}

object Value {
  type Data = terms.Term
  def unapply(arg: Pattern)(implicit ext: Extractor): Option[Data] = ext.unapplyValue(arg)
}

object Bind {
  type Data = (names.TermName, Pattern)
  def unapply(arg: Pattern)(implicit ext: Extractor): Option[Data] = ext.unapplyBind(arg)
}

object Unapply {
  type Data = (terms.Term, List[terms.Term], List[Pattern])
  def unapply(arg: Pattern)(implicit ext: Extractor): Option[Data] = ext.unapplyUnapply(arg)
}

object Alternative {
  type Data = List[Pattern]
  def unapply(arg: Pattern)(implicit ext: Extractor): Option[Data] = ext.unapplyAlternative(arg)
}

object TypeTest {
  type Data = typetrees.TypeTree
  def unapply(arg: Pattern)(implicit ext: Extractor): Option[Data] = ext.unapplyTypeTest(arg)
}

object Wildcard {
  def unapply(arg: Pattern)(implicit ext: Extractor): Boolean = ext.unapplyWildcard(arg)
}