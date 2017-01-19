
object Test {
  object TestSeq {
    def unapplySeq(x: Int): Option[Seq[Int]] = Some(List(x, x, x))
  }

  def main(args: Array[String]): Unit = {
    42 match {
      case TestSeq(42, x, 42) => System.out.println(x)
    }
  }
}
