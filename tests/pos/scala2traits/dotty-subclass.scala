// This is supposed to be compiled by Dotty
class Sub extends T

trait DT {

  lazy val dx = 2

}

class A extends S2T with S2Tprivate with DT {
  val a: Int = 3
  var b = 2
}

object Main {
  def main(args: Array[String]): Unit = {
    val sub = new Sub
    println(sub.d)
    println(sub.v)
    println(sub.O)
    println(sub.w)

    val a = new A
    a.x += a.y
    println(a.x)
    println(a.f(a.a + a.b))

    a.xx += a.yy
    println(a.x)
    println(a.ff(a.xx))
  }
}

