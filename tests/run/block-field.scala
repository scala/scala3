case class C(i: Int)

class M:
  var i: Int = 0

object Test:
  def main(args: Array[String]): Unit =
    println({ val c = C(42); println("nested"); c}.i)
    val m = M()
    { println("m"); m }.i = 1
    println(m.i)
