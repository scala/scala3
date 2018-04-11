package scala.tasty.pattern

import scala.tasty.term.Term
import scala.tasty._

trait Pattern extends Positioned {
  def tpe: Type
}

object Value {
  type Data = Term
  def unapply(arg: Pattern)(implicit ext: Extractor): Option[Data] = ext.unapplyValue(arg)
}

object Bind {
  type Data = (scala.tasty.TermName, Pattern)
  def unapply(arg: Pattern)(implicit ext: Extractor): Option[Data] = ext.unapplyBind(arg)
}

object Unapply {
  type Data = (scala.tasty.term.Term, List[scala.tasty.term.Term], List[Pattern])
  def unapply(arg: Pattern)(implicit ext: Extractor): Option[Data] = ext.unapplyUnapply(arg)
}

object Alternative {
  type Data = List[Pattern]
  def unapply(arg: Pattern)(implicit ext: Extractor): Option[Data] = ext.unapplyAlternative(arg)
}

object TypeTest {
  type Data = scala.tasty.typetree.TypeTree
  def unapply(arg: Pattern)(implicit ext: Extractor): Option[Data] = ext.unapplyTypeTest(arg)
}

object Wildcard {
  def unapply(arg: Pattern)(implicit ext: Extractor): Boolean = ext.unapplyWildcard(arg)
}