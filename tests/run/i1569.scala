object Test {
  transparent def foo(n: => Int & Constant) = n + n

  def main(args: Array[String]): Unit = foo({ println("foo"); 42 })
}
