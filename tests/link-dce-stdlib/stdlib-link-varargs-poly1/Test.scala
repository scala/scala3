import scala.annotation.internal

object Test {
  def main(args: Array[String]): Unit = {
    foo(1)
  }

  def bar[T](x: T) = foo(x)

  def foo(ns: Any*): Unit = {
    System.out.println(42)
  }

}
