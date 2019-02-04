object Test {

  case class C(x: Int)

  def f(x: Int) given (c: C) = x + c.x

  def g(x: Int) given (c: C) (y: Int) = x + c.x + y

  def h(x: Int) given () = x // error

  implicit object C extends C(11)

  f(1)
  f(1) given C
  f given 2  // error
  f(1)(C)   // error

  g(1)(2) // OK
  (g(1) given C)(2) // OK
  g(1) given 2 // error
  g(1) given C given 2 // error
  g(1)(C)(2) // error
  g(1)(C) given 2 // error
}