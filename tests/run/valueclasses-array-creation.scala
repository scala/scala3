class X(val x: Int) extends AnyVal
object Test {
  def main(args: Array[String]) = {
    val simple = Array(new X(11), new X(22))
    val a = Array(Array(new X(1), new X(2)), Array(new X(3)))
    val b = a(0)
    val c = b(0)
    val d = a(0)(0)
    a(0)(0) = new X(7)
    def test(x: Array[Array[X]]) = {
      x(0) = Array(new X(5));
      x
    }
    def test2(x: Array[X]) = x
    def test3[T](y: Array[T]) = {
      val a = y(0);
      y(0) = y(1);
      y(1) = a;
      y match {
        case ar@Array(el1: X, el2: X) => (ar.asInstanceOf[Array[X]]) filter (_.x > 1) map { t => new X(el2.x * 2) }
        case _ => y
      }
    }
    def test4(y: Array[X]) = {
      y match {
        case ar@Array(el1, el2) => ar filter (_.x > 1) map { t => new X(el2.x * 2) }
        case _ => y
      }
    }


    val x = new Array[X](3)
    val y = new Array[Array[X]](3)
    val z = new Array[Array[Array[X]]](3)

    println(simple.toList)

    println(a.toList)
    println(b.toList)
    println(c)
    println(d)
    println(test(a).toList)
    println(test2(b).toList)

    println(x.toList)
    println(y.toList)
    println(z.toList)

    println(test3(Array(new X(3), new X(5))).toList)
    println(test4(Array(new X(0), new X(7))).toList)
  }
}