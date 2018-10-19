object Test {
  inline def foo(inline n: => Int) = n + n

  def main(args: Array[String]): Unit = foo({ println("foo"); 42 })
}
