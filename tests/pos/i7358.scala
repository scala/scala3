package test

import scala.quoted._
import scala.compiletime._

inline def summonT[Tp <: Tuple] <: Tuple = inline erasedValue[Tp] match {
  case _ : Unit => ()
  case _ : (hd *: tl) => {
    type H = hd
    summonFrom {
      case given _ : TypeTag[H] => summon[TypeTag[H]] *: summonT[tl]
    }
  }
}

def test[T : TypeTag] = summonT[Tuple1[List[T]]]
