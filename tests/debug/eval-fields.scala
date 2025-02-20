object Test:
  def main(args: Array[String]): Unit =
    val a = new A("a", 1)
    println(a)

class A(name: String, val n: Int):
  val a1 = s"$name.a1"
  private val a2 = s"$name.a2"

  object B:
    val  b1 = s"$name.B.b1"

  private object C:
    val c1 = s"$name.C.c1"

  override def toString: String =
    name + a2
