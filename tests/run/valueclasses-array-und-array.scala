import scala.collection.mutable._
class X(val x: Array[Y]) extends AnyVal
class X0(val x: Array[Y0]) extends AnyVal
class X00(val x: Array[Array[Y0]]) extends AnyVal
class X2(val x: Array[Y2]) extends AnyVal
class X3(val x: Array[Y3]) extends AnyVal
class Y3(val x: Int) extends AnyVal
class Y0(val a: Int) extends AnyVal
class Y(val a: Array[X])
class Y2(val a: Int) extends AnyVal
class Z(val y: Y) extends AnyVal

object Test {
  def main(args: Array[String]) = {
    val ar0 = new Array[X](3)
    ar0(0) = new X(Array(new Y(Array(new X(Array())))))
    ar0(1) = new X(new Array[Y](4))
    val ar01 = new Array[X00](5)
    val ar011: Array[X00] = null
    val ar02 = Array(new X00(Array(Array(new Y0(7)))))

    val y1 = Array(new Y(Array(new X(Array(new Y(new Array[X](2)), new Y(Array(new X(new Array[Y](4)))))))))
    val y2 = y1(0)

    val z1 = Array(Array(new Y(Array(new X(Array(new Y(new Array[X](2)), new Y(Array(new X(new Array[Y](4))))))))))
    val z2 = z1(0)
    val z3 = z1(0)(0)
    val z4 = z2(0)
    println(z1.size)
    println(z2.size)

    val y10 = Array(new X0(Array(new Y0(4), new Y0(3))))
    val y11 = Array(new X0(new Array[Y0](5)))
    val y20 = y10(0)

    val z10 = Array(Array(new X0(Array(new Y0(4), new Y0(3)))))
    val z11 = Array(Array(new X0(new Array[Y0](5))))
    val z20 = z10(0)
    val z30 = z10(0)(0)
    val z40 = z20(0)

    z10 map (x => Array(x(0)))
    y10 map (x => x)

    val ar = Array(new X0(Array(new Y0(1))))

    val ar2 = Array(new X(Array(new Y(Array[X](new X( new Array[Y](5)))))))

    val t = new X3(Array(new Y3(1)))
    t.toString
    val t2 = new X3(new Array[Y3](4))

    val ar3 = Array(new X2(new Array[Y2](3)))
    val ar4 = Array(new X2(Array(new Y2(3))))
  }
}