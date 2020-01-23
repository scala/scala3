object Test {

  case class C(x: Int)
  case class D(x: Int)

  def f(x: Int) with (c: C) = x + c.x

  def g0(x: Int) with (c: C) (y: Int) = x + c.x + y

  def g(x: Int) with (c: C) with D = x + c.x + summon[D].x  // OK

  def h(x: Int) given () = x // error: missing return type

  given C as C(11)
  given D as D(11)

  f(1)
  f(1).with(C)
  f.with(2)  // error
  f(1)(C)   // error

  g(1)    // OK
  g(1).with(C)   // OK
  g(1).with(C).with(D(0)) // OK
  g(1).with(D) // error
  g(1)(D) // error
  g(1)(C)(D) // error
}