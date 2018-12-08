
import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    type I = Int
    printType[Int]
    printType[List[String]]
    printType[Map[String, Int]]
    printType[Map[String, I]]
  }
}
