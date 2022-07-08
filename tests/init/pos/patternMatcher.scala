import scala.collection.mutable

class Translater:
  val count = new mutable.HashMap[Int, Int] {
    override def default(key: Int) = 0
  }
  count.get(10)
  val n = 10
