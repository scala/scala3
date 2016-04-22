import scala.collection.mutable._

class X(val a: Int) extends AnyVal {
  override def toString = s"X-$a"
}
object Test {
  def prettyPrintArray3(x: Array[_]) = { println(x.toString); println(x(0)) }

  def main(args: Array[String]): Unit = {
    val r1 = for (y <- test) { println(y) }
    val r2 = test foreach ( x => println(x.toString) )
    val r3 = test.foldLeft(new X(5))((x, y) => new X(x.a + y.a))
    val r4 = test filter ( x => x.toString.contains("1") )
    val r5 = r4.toList
    println(r3)
    println(r5)
    val r6 = test.toSet
    println(s"r6: $r6")
    val r7 = test.toList
    println(s"r7: $r7")

    val t1: ArrayOps[X] = Array(new X(7))
  }

  def test: Array[X] = Array(new X(7), new X(9), new X(11))
}