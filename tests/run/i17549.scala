trait T:
  final val x: 1 =
    println("T.x")
    1
end T

trait U:
  def x: Any
  def y: Any

class C extends T with U:
  final val y: 2 =
    println("C.y")
    2
end C

object Test:
  def main(args: Array[String]): Unit =
    val c = new C
    println(c.x)
    println(c.y)

    val u: U = c
    println(u.x)
    println(u.y)
  end main
end Test
