object Test {

  case class C(x: Int)
  case class D(x: Int)

  def f(x: Int)(using c: C) = x + c.x

  def g0(x: Int)(using c: C) (y: Int) = x + c.x + y

  def g(x: Int)(using c: C)(using D) = x + c.x + summon[D].x  // OK

  def h(x: Int) given () = x // error: missing return type

  given C: C(11)
  given D: D(11)

  f(1)
  f(1)(using C)
  f(using 2)  // error
  f(1)(C)   // error

  g(1)    // OK
  g(1)(using C)   // OK
  g(1)(using C)(using D(0)) // OK
  g(1)(using D) // error
  g(1)(D) // error
  g(1)(C)(D) // error
}