import scala.quoted._
import scala.compiletime._

transparent inline def summonT[Tp <: Tuple](using s: Scope): Tuple = inline erasedValue[Tp] match {
  case _ : EmptyTuple => Tuple()
  case _ : (hd *: tl) => {
    type H = hd
    summonFrom {
      case given _ : s.Type[H] => summon[s.Type[H]] *: summonT[tl]
    }
  }
}

def test[T](using s: Scope)(using s.Type[T]) = summonT[Tuple1[List[T]]]
