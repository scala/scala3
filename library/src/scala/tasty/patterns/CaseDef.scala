package scala.tasty.patterns

import scala.tasty.terms.Term
import scala.tasty.{Extractor, Positioned}

trait CaseDef extends Positioned

object CaseDef {
  type Data = (Pattern, Option[Term], Term)
  def unapply(arg: CaseDef)(implicit ext: Extractor): Option[Data] = ext.unapplyCaseDef(arg)
}
