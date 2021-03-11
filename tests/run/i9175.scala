import scala.collection.immutable.SortedMap

object Test {

  def main(args: Array[String]): Unit = {
    val sortedMap = SortedMap("a" -> 1, "b" -> 2, "c" -> 3)
    val empty     = sortedMap.empty
    println(empty)
  }
}
