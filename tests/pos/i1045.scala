import scala.collection._
object T {
  val newSymbolMap: mutable.HashMap[String, mutable.HashMap[Int, Double]] = mutable.HashMap.empty
  val map = newSymbolMap.getOrElse("a", mutable.HashMap.empty)
  map.put(1, 0.0)
  newSymbolMap.put("a", map)
}
