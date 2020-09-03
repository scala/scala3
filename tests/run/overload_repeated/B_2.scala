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
    // Java, Scala 2 and Dotty all agree that Java varargs are
    // less specific than Java non-varargs:
    assert(A_1.check())
    assert(A_1.foo1("") == 1) // Same as in Java and Scala 2
    assert(A_1.foo2("") == 1) // Same as in Java and Scala 2
    assert(A_1.foo3("") == 1) // Same as in Java and Scala 2
    assert(A_1.foo4("") == 1) // Same as in Java and Scala 2

    // ... but Scala 2 seems to treat Scala varargs specially
    // (or maybe it's Object coming from Java which is treated
    // specially), in Dotty this doesn't make a difference:
    assert(bar1("") == 1) // ambiguous in Scala 2
    assert(bar2("") == 1) // same in Scala 2
    assert(bar3("") == 1) // same in Scala 2
    assert(bar4("") == 1) // same in Scala 2
  }
}
