
import Macros.*

object Test {

  def main(args: Array[String]): Unit = {

    matches[Int, Int]
    matches[1, Int]
    matches[Int, 2]

    matches[List[Int], List[Int]]
    matches[List[Int], List[Double]]

  }
}

