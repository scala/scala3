import scala.compiletime.requireConst

object Test {

  requireConst(true)
  requireConst(1)
  requireConst(1L)
  requireConst(1d)
  requireConst(1f)
  requireConst('a')
  requireConst("abc")

  requireConst(1 + 3)
  requireConst("abc" + "cde")

  val a: Int = 2
  inline val b = 2

  requireConst(a) // error: expected a requireConstant value but found: Test.a
  requireConst(b)
  requireConst(b + b)
  requireConst(b - b)
  requireConst(b / b)
  requireConst(b % b)

  inline def f(inline n: Int): Int = 4 + n

  requireConst(f(1))
  requireConst(f(a)) // error: expected a requireConstant value but found: 4.+(Test.a):Int
  requireConst(f(b))
  requireConst(f(b + b))

  def g(n: Int): n.type = n

  requireConst(g(1))
  requireConst(g(a)) // error: expected a requireConstant value but found: Test.a
  requireConst(g(b))


  inline def twice(inline n: Int): Int =
    requireConst(n) // static assertion that n is a requireConstant
    n + n

  twice(1)
  twice(a) // error: expected a requireConstant value but found: Test.a
  twice(b)

}
