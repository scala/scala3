package scala
import annotation.showAsInfix

sealed trait Tuple extends Any

@showAsInfix
sealed class *:[+H, +T <: Tuple] extends Tuple {
  @`rewrite` def head: H = ???
  @`rewrite` def tail: T = ???
}

object *: {
  @`rewrite` def unapply[H, T <: Tuple](x: H *: T) = Some((x.head, x.tail))
}
