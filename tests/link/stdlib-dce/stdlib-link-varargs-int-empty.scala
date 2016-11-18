
object Test {
  def main(args: Array[String]): Unit = {
    foo()
  }

  def foo(ns: Int*): Unit = {
    System.out.println(42)
  }
}
