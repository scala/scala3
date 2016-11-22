
object Test {
  def main(args: Array[String]): Unit = {
    foo(new Object)
  }

  def foo(ns: AnyRef*): Unit = {
    System.out.println(42)
  }
}
