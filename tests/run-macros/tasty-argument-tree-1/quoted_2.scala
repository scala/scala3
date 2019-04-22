
import Macros.inspect

object Test {
  val a: Int = 4
  def b: Int = 5

  def main(args: Array[String]): Unit = {
    val v: Int = 1
    def d: Int = 2
    lazy val l: Int = 3
    def d2(): Int = 2
    def d3(a: Int): Int = a
    def d4[T]: Int = 2
    inspect(3)
    inspect(v)
    inspect(d)
    inspect(l)
    inspect(a)
    inspect(b)
    inspect(d2())
    inspect(d3(3))
    inspect(d4[Int])

    val vv = v
    def dv = v
    val vd = d
    def dd = d
    inspect(vv)
    inspect(dv)
    inspect(vd)
    inspect(dd)

    inspect((dv, vd))

  }
}
