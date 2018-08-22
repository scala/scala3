class A {
  def odd(x: Int) = if (x == 0) false else !even(x-1)
  def even(x: Int) = {
    val foo = {
      if (x == 0) true else !odd(x-1) // error: overloaded or recursive method needs result type
    }
    false
  }
}
