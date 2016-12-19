import scala.annotation.internal

object Test {

  def main(args: Array[String]): Unit = {
    foo()
  }

  def foo(ns: Any*): Unit = {
    System.out.println(42)
  }
}
