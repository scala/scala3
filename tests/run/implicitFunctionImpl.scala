
object Test {
  def main(args: Array[String]): Unit = {
    println(new Fun().apply(42))
  }
}

class Fun extends ImplicitFunction1[Int, Int] {
  def apply(implicit v1: Int): Int = v1
}
