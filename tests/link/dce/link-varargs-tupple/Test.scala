
object Test {
  def main(args: Array[String]): Unit = {
    //    scala.collection.immutable.Map[Int, Int]((1, 2), (3, 4), (5, 6))
    tupples((1, 2))
  }
  def tupples[A, B](elems: (A, B)*): Unit = ()
}
