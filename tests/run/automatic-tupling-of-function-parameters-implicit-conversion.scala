
object Test {

  implicit def F2ToT2(f: Function2[Int, Int, Int]): Tuple2[Int, Int] => Int = {
    ???
    x => f(x._1, x._2)
  }

  def main(args: Array[String]): Unit = {
    val f: Tuple2[Int, Int] => Int = (x: Int, y: Int) => x + y
    f((3, 4))
  }

}
