object Test {
  new Thread(() => println("hi"))

  def foo(x: Int => Int, y: Int): Int = 1
  def foo(x: Int, y: Int): Int = 2
  foo(1, 2)
  foo(x => x, 2)
}
