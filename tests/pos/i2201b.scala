trait X
trait Y

object Test {
  type One[A <: X, B <: Y]

  type Two[TA <: Y, TB <: X] = One[TB, TA]

  def foo[M[_ <: Y, _ <: X]](x: M[_ <: Y, _ <: X]) = x

  val a: Two[Y, X] = ???

  foo(a)
}
