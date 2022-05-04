package dotty.tools.scaladoc

import scala.scalajs.js

class RecentQuery(val query: String, val timestamp: Double) extends js.Object

object RecentQueryStorage extends SafeLocalStorage[js.Array[RecentQuery]]("__RECENT__QUERIES__", js.Array()) {
  val maxEntries = 5

  def addEntry(rq: RecentQuery): Unit = {
    val newData = getData :+ rq
    setData(newData.sortBy(_.timestamp).distinctBy(_.query).takeRight(maxEntries))
  }
}
