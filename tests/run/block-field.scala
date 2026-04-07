case class C(i: Int)

object Test:
  def main(args: Array[String]): Unit =
    println({ val c = C(42); println("nested"); c}.i)
