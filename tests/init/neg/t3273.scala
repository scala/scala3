import scala.language.implicitConversions

class Test {
  val num1: LazyList[Int] = 1 #:: num1.map(_ + 1)                        // error
  val num2: LazyList[Int] = 1 #:: num2.iterator.map(_ + 1).to(LazyList)  // error

  def main(args: Array[String]): Unit = {
    val x1 = (num1 take 10).toList
    val x2 = (num2 take 10).toList
    assert(x1 == x2)
  }
}
