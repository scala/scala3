object Test {

  case class C(x: Int)

  def f(x: Int) with (c: C) = x + c.x

  def g(x: Int) with (c: C) (y: Int) = x + c.x + y

  implicit object C extends C(11)

  f(1)
  f(1) with C
  f with 2  // error
  f(1)(C)   // error

  g(1)(2) // OK
  (g(1) with C)(2) // OK
  g(1) with 2 // error
  g(1) with C with 2 // error
  g(1)(C)(2) // error
  g(1)(C) with 2 // error
}