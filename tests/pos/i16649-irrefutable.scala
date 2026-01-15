//> using options -Werror -deprecation -feature

import quoted.*

def foo(using Quotes)(x: Expr[Int]) =
  val '{ $y } = x
  val '{ $a: Any } = x
  val '{ $b: Int } = x
  val '[List[Int]] = Type.of[List[Int]]
