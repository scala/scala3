
// Test that by-name varargs don't crash at runtime.
object Test {
  def foo(x: => Any*): Unit = x

  def main(args: Array[String]): Unit = {
    foo("hello")
    foo(42)
    foo("a", "b", 200)
  }
}
