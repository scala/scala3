import scala.annotation.internal

object Test {
  def main(args: Array[String]): Unit = {
    scala.collection.immutable.Map.empty[Int, Int]
    scala.collection.immutable.Map[Int, Int]()
  }
}
