package dotty.tools.scaladoc

import scala.scalajs.js

class RecentQuery(val query: String, val timestamp: Double) extends js.Object

object RecentQueryStorage extends SafeLocalStorage[js.Array[RecentQuery]]("__RECENT__QUERIES__", js.Array()) {
  val maxEntries = 3

  def addEntry(rq: RecentQuery): Unit = {
    if !getData.exists(_.query.contains(rq.query)) then
      val newData = getData.filter(q => !rq.query.contains(q.query)) :+ rq
      setData(newData.sortBy(_.timestamp).reverse.distinctBy(_.query).take(maxEntries))
  }
}
