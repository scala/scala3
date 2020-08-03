package test

import scala.quoted._
import scala.compiletime._

transparent inline def summonT[Tp <: Tuple](using QuoteContext): Tuple = inline erasedValue[Tp] match {
  case _ : EmptyTuple => Tuple()
  case _ : (hd *: tl) => {
    type H = hd
    summonFrom {
      case given _ : Staged[H] => summon[Staged[H]] *: summonT[tl]
    }
  }
}

def test[T : Staged](using QuoteContext) = summonT[Tuple1[List[T]]]
