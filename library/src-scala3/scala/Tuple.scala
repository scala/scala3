package scala
import annotation.showAsInfix

object typelevel {
  erased def erasedValue[T]: T = ???
}

import typelevel._

sealed trait Tuple extends Any

@showAsInfix
sealed class *:[+H, +T <: Tuple] extends Tuple {
  rewrite def head: H = ???
  rewrite def tail: T = ???

  rewrite private def _size(xs: Tuple): Int = //rewrite
    xs match {
      case _: Unit => 0
      case _: *:[_, xs1] => _size(erasedValue[xs1]) + 1
    }
}

object *: {
  rewrite def unapply[H, T <: Tuple](x: H *: T) = Some((x.head, x.tail))
}
