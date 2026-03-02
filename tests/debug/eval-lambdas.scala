object Test:
  val a = 1
  private val b = 2
  def main(args: Array[String]): Unit =
    val c = 3
    println(a + b + c)
    (new A).m()

class A:
  val a = 1
  private val b = 2
  def m() =
    val c = 3
    println(a + b + c)
