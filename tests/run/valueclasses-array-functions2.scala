class X(val x: Int) extends AnyVal {
  override def toString = s"X($x)"
}
class Y(val y: Int)
class Z(val z: String) extends AnyVal {
  override def toString = s"Z($z)"
}

object Test {
  def main(args: Array[String]) = {
    val r = test map { v1 => new X(v1.x*2) }
    println(s"r: ${r.toList}")
    val r3 = Array(1,2,3).map(v3 => new X(v3))
    println(s"r3: ${r3.toList}")
    val r4 = test2 map (v4 => new X(v4.y))
    println(s"r4: ${r4.toList}")
    val r5 = test3 map {x => new X(x.asInstanceOf[X].x*3)}
    println(s"r5: ${r5.toList}")
    val r6 = test2 map (v6 => new Z(v6.y.toString))
    println(s"r6: ${r6.toList}")
    val r7 = test map (v7 => new Z(v7.toString))
    println(s"v7: ${r7.toList}")
  }
  def test = Array(new X(3), new X(4), new X(5))
  def test2 = Array(new Y(1), new Y(2), new Y(3))
  def test3: Array[_] = Array(new X(1), new X(2), new X(3))
}