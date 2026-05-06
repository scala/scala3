import caps.fresh

class A

object Test:
  def foo(x: A^{fresh}) // error
    : A^{fresh}  // error
    = x
  val a: A^{fresh} = ??? // error
  type X = A^ -> A^{fresh}// OK
  type Y = List[A^{fresh}] // error

  def bar[X^]: List[A^{X}] = ???
  val b = bar[{fresh}]  // error

