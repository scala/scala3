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
    val r7 = test.toList
    println(s"r7: $r7")

    val t1: ArrayOps[X] = Array(new X(7))
    //def a1 = Array(Array[X]())
    def a2 = Array.ofDim[X](2)
    def a3 = new Array[X](5)
    def a4: Array[X] = Array(new X(3), new X(4), new X(5))
    println(a4.toArray.toList)
    println(a4.:+(new X(6)).toList)
    println(a4.+:(new X(7)).toList)
    println(a4.par.toList)
    val a5 = Array(new X(1), new X(2))
    val a6 = Array(new X(3), new X(4))
    println(a5.map(x => a6(0)).toList)
  }

  def test: Array[X] = Array(new X(7), new X(9), new X(11))
}