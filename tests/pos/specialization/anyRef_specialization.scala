object Test {
  def foo[@specialized(AnyRef) T](t: T): T = t
  def main (args: Array[String]) = {
    foo(5)
  }
}
