trait A {
  def a: Int
}

object Test {
  def f(a: Int) = new A {
// TODO NEEDS MANUAL CHANGE (early initializers)
// BEGIN copied early initializers
private[this] val b = a
// END copied early initializers

    def a = b
  }

  def main(args: Array[String]): Unit = {
    println(f(7).a)
  }
}
