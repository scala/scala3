
object Test {
  object Twice {
    def unapply(x: Int): Option[Int] = if (x % 2 == 0) Some(x / 2) else None
  }

  def main(args: Array[String]): Unit = {
    val Twice(x) = 84
    System.out.println(x)
  }
}
