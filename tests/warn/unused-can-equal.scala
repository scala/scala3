
//> using options -Werror -Wunused:all

import scala.language.strictEquality

class Box[T](x: T) derives CanEqual:
  def y = x

def f[A, B](a: A, b: B)(using CanEqual[A, B]) = a == b // no warn

def g =
  import Box.given // no warn
  "42".length

@main def test() = println:
  Box(1) == Box(1L)
