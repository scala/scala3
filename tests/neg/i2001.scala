class A {
  def odd(x: Int) = if (x == 0) false else !even(x-1)
  def even(x: Int) = if (x == 0) true else !odd(x-1) // error: overloaded or recursive method needs result type

  lazy val x = x // error: recursive value needs type
}
