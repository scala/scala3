import scala.reflect.ClassTag

object Test {
  def main(args: Array[String]): Unit = {

    def testArray[T: ClassTag](n: Int, elem: Int => T): Unit = {
      val t: Tuple = Tuple.fromArray(Array.tabulate(n)(elem))
      println(t)
    }

    for (i <- 0 to 25)
      testArray(i, j => j)

    for (i <- 0 to 25)
      testArray(i, j => ("x" + j))

    for (i <- 0 to 25)
      testArray(i, j => if (j % 2 == 0) j else ("x" + j))
  }
}
