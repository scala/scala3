package test

import scala.quoted._
import scala.compiletime._

transparent inline def summonT[Tp <: Tuple](using QuoteContext): Tuple = inline erasedValue[Tp] match {
  case _ : EmptyTuple => Tuple()
  case _ : (hd *: tl) => {
    type H = hd
    inline summonInlineOpt[Type[H]] match {
      case Some(given Type[H]) => summon[Type[H]] *: summonT[tl]
    }
  }
}

def test[T : Type](using QuoteContext) = summonT[Tuple1[List[T]]]
