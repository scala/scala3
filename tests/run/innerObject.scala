object Test {
  def foo(x: Int) = {
    println(x)
  }

  def outer(x: Int) = {
    def inner() = {
      foo(x)
    }
    inner()
  }

  def outer2(x: Int) = {
    def inner2() = {
      Test.foo(x)
    }
    inner2()
  }

  def main(args: Array[String]): Unit = {
    outer(1)
    outer2(2)
  }
}
