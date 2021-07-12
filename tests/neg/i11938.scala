import java.util.function.Function

object Test {
  def foo[V](v: V): Int = 1
  def foo[U](fn: Function[Int, U]): Int = 2

  def main(args: Array[String]): Unit = {
    val f: Int => Int = x => x
    foo(f) // error
      // Like Scala 2, we emit an error here because the Function1 argument was
      // deemed SAM-convertible to Function, even though it's not a lambda
      // expression and therefore not convertible. If we wanted to support this,
      // we would have to tweak TestApplication#argOK to look at the shape of
      // `arg` and turn off SAM conversions when it's a non-closure tree.
  }
}
