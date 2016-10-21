
object Test {
  def main(args: Array[String]): Unit = {
    foo("a", "b", "c")
  }

  def foo(ns: Any*): Unit = {
    println(42)
  }
}
