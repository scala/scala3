import scala.compiletime.*

object Test {
  transparent inline def summonValues[T]: Tuple = inline erasedValue[T] match {
    case _: EmptyTuple => EmptyTuple
    case _: (a *: b) => constValue[a] *: summonValues[b]
  }

  summonValues[(96, 97, 98)]
}
