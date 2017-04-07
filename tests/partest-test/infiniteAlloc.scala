import scala.collection.mutable
object Test {
  val map = mutable.Map.empty[String, String]

  def main(args: Array[String]): Unit = while (true) {
    val time = System.currentTimeMillis.toString
    map += (time -> time)
  }
}
