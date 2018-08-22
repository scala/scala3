class A {
  def odd(x: Int) = if (x == 0) false else !even(x-1)
  def even(x: Int) = {
    def foo = {
      if (x == 0) true else !odd(x-1) // error: overloaded or recursive method needs result type
    }
    foo
  }
  lazy val x = {
    def foo = x // error
    foo
  }
}
