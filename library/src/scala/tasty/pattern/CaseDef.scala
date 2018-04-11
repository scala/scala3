package scala.tasty.pattern

import scala.tasty.term.Term
import scala.tasty.{Extractor, Positioned}

trait CaseDef extends Positioned

object CaseDef {
  type Data = (Pattern, Option[Term], Term)
  def unapply(arg: CaseDef)(implicit ext: Extractor): Option[Data] = ext.unapplyCaseDef(arg)
}
