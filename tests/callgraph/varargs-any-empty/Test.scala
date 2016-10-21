
object Test {
  def main(args: Array[String]): Unit = {
    foo()
  }

  def foo(ns: Any*): Unit = {
    println(42)
  }
}
