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

  // https://github.com/scala/scala3/issues/24072
  def bar5[T](a: Class[? <: T]): Int = 1
  def bar5[T](a: Class[T], ints: Int*): Int = 2

  def bar6[T](a: Class[T], ints: Int*): Int = 1
  def bar6[T](a: Int): Int = 2

  def bar7[T <: Number](a: Class[? <: T]): Int = 1
  def bar7[T <: Number](a: Class[T], ints: Int*): Int = 2

  def bar8[T <: Number](a: Class[? <: T]): Int = 1 // (a)
  def bar8[T](a: Class[T], ints: Int*): Int = 2 // (b)

  def bar9[T](a: Class[? <: T]): Int = 1 // (a)
  def bar9[T <: Number](a: Class[T], ints: Int*): Int = 2 // (b)

  def main(args: Array[String]): Unit = {
    // In Java, varargs are always less specific than non-varargs (see
    // https://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.12.2),
    // this isn't true in Scala which leads to `foo1` being ambiguous.
    assert(A_1.check())
    // assert(A_1.foo1("") == 1) // Works in Java, ambiguous in Scala 2 and Dotty
    assert(A_1.foo2("") == 1) // Same as in Java and Scala 2
    assert(A_1.foo3("") == 1) // Same as in Java and Scala 2
    assert(A_1.foo4("") == 1) // Same as in Java and Scala 2
    assert(A_1.foo5(classOf[Object]) == 1) // Same as in Java and Scala 2
    assert(A_1.foo6(classOf[Object]) == 1) // Same as in Java and Scala 2
    assert(A_1.foo7(classOf[Integer]) == 1) // Same as in Java and Scala 2
    assert(A_1.foo8(classOf[Integer]) == 1) // Same as in Java and Scala 2
    // assert(A_1.foo9(classOf[Integer]) == 1) // Works in Java, ambiguous in Scala 2 and 3

    // Same with Scala varargs:
    // assert(bar1("") == 1) // Works in Java, ambiguous in Scala 2 and Dotty
    assert(bar2("") == 1) // same in Scala 2
    assert(bar3("") == 1) // same in Scala 2
    assert(bar4("") == 1) // same in Scala 2
    assert(bar5(classOf[Object]) == 1) // same in Scala2
    assert(bar6(classOf[Object]) == 1) // same in Scala2
    assert(bar7(classOf[Integer]) == 1) // same in Scala2
    assert(bar8(classOf[Integer]) == 1) // same in Scala2
    // assert(bar9(classOf[Integer]) == 1) Works in Java, ambiguous in Scala 2 and 3
  }
}
