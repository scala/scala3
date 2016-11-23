
object Test {
  def main(args: Array[String]): Unit = {
    foo(1, 2, 3)
  }

  def foo(ns: Int*): Unit = {
    System.out.println(42)
  }
}
