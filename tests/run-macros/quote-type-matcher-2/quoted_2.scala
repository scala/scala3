
import Macros.*

object Test {

  def main(args: Array[String]): Unit = {

    println(lift[Int])
    println(lift[List[Int]])
    println(lift[Option[Int]])
    println(lift[Int => Double])

  }
}
