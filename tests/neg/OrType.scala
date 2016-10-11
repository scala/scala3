class B(val x: Int)
class C(val x: Double)

object Test{
  def bar(x: B | C): Int | Double = x.x  // error
  def main(args: Array[String]): Unit = {
    val b = new B(1)
    val c = new C(1)
    bar(if (b.hashCode > c.hashCode) b else c)
  }
}
