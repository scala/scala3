class X(val a: Int) extends AnyVal {
  override def toString = s"X-$a"
}
class Y(val b: Int)
object Test {
  def main(args: Array[String]) = {
    val t: Array[X] = Array(new X(5))
    val t2 = new Array[X](2)
    t2(0) = new X(1)
    t2(1) = new X(2)
    println(t.size)
    println(t2.size)
    println(test.size)
    t: Array[X]
    t2: Array[X]
  }
  def test = {
    val a = Array(new X(5), new X(6), new X(7))
    a
  }
}