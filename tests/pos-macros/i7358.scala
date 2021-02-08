package test

import scala.quoted.*
import scala.compiletime.*

transparent inline def summonT[Tp <: Tuple](using Quotes): Tuple = inline erasedValue[Tp] match {
  case _ : EmptyTuple => Tuple()
  case _ : (hd *: tl) => {
    type H = hd
    summonFrom {
      case given Type[H] => summon[Type[H]] *: summonT[tl]
    }
  }
}

def test[T : Type](using Quotes) = summonT[Tuple1[List[T]]]
