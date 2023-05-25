import quoted.*

def foo(using Quotes)(x: Expr[Int]) =
  val '{ ($y: Int) + ($z: Int) } = x // error
