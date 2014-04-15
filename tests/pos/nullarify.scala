object Test {

  def foo: Int = 2

  println(foo)

  def bar(x: => Int) = x + baz(x)

  def baz(y: => Int) = y

  bar(foo)

}
