// scalajs: --skip

object Test {
  def bar1(x: Any) = 1
  def bar1(x: String*) = 2

  def bar2(x: Any) = 1
  def bar2(x: Any*) = 2

  def bar3[T](x: T): Int = 1
  def bar3[T](x: T*): Int = 2

  def bar4[T](x: T): Int = 1
  def bar4[T](x: T, xs: T*): Int = 2

  def main(args: Array[String]): Unit = {
    // In Java, varargs are always less specific than non-varargs (see
    // https://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.12.2),
    // this isn't true in Scala which leads to `foo1` being ambiguous.
    assert(A_1.check())
    // assert(A_1.foo1("") == 1) // Works in Java, ambiguous in Scala 2 and Dotty
    assert(A_1.foo2("") == 1) // Same as in Java and Scala 2
    assert(A_1.foo3("") == 1) // Same as in Java and Scala 2
    assert(A_1.foo4("") == 1) // Same as in Java and Scala 2

    // Same with Scala varargs:
    // assert(bar1("") == 1) // Works in Java, ambiguous in Scala 2 and Dotty
    assert(bar2("") == 1) // same in Scala 2
    assert(bar3("") == 1) // same in Scala 2
    assert(bar4("") == 1) // same in Scala 2
  }
}
