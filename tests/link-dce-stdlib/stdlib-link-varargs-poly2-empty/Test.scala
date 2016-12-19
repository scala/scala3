import scala.annotation.internal

object Test {
  def main(args: Array[String]): Unit = {
    foo(1)
  }

  def bar[T](x: T) = foo()

  def foo[U](ns: U*): Unit = {
    System.out.println(42)
  }

}
