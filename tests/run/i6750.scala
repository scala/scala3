
object Test {

  inline def foo(i: => Int)  = i + i
  inline def foo(l: => Long) = l * l

  inline def bar(i: () => Int)  = ???
  inline def bar[T](x: () => T) = ???

  def main(args: Array[String]): Unit = {
    assert(10 == foo(5))
    assert(25L == foo(5L))

  }

}
