package scala
import annotation.showAsInfix

sealed trait Tuple extends Any

@showAsInfix
sealed class *:[+H, +T <: Tuple] extends Tuple {
  def head: H = ???
  def tail: T = ???
}

object *: {
  def unapply[H, T <: Tuple](x: H *: T) = Some((x.head, x.tail))
}
