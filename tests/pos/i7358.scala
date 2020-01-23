package test

import scala.quoted._
import scala.compiletime._

inline def summonT[Tp <: Tuple] with QuoteContext <: Tuple = inline erasedValue[Tp] match {
  case _ : Unit => ()
  case _ : (hd *: tl) => {
    type H = hd
    summonFrom {
      case given _ : Type[H] => summon[Type[H]] *: summonT[tl]
    }
  }
}

def test[T : Type] with QuoteContext = summonT[Tuple1[List[T]]]
