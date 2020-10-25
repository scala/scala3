package dotty.tools
package dotc
package util

import scala.annotation.internal.sharable

import core.Contexts._
import collection.mutable

@sharable object Stats {

  final val enabled = false

  var monitored: Boolean = false

  @volatile private var stack: List[String] = Nil

  val hits: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int] {
    override def default(key: String): Int = 0
  }

  inline def record(inline fn: String, inline n: Int = 1): Unit =
    if (enabled) doRecord(fn, n)

  def doRecord(fn: String, n: Int) =
    if (monitored) {
      val name = if (fn.startsWith("member-")) "member" else fn
      hits(name) += n
    }

  def doRecordSize(fn: String, coll: scala.collection.Iterable[_]): coll.type =
    doRecord(fn, coll.size)
    coll

  def doRecordListSize[T](fn: String, coll: List[T]): coll.type =
    val name = fn.drop("listSize/".length).takeWhile(_ != '@')
    val adjustedSize =
      if name == "extension_tail" then coll.size - 1
      else if name == "extension_::" then coll.size + 1
      else coll.size
    if adjustedSize > 1 then
      doRecord(fn, coll.size + 5) // 5 words overhead for arrays: 3 words header + length + elementTag
    coll

  def doRecordBufferSize(fn: String, coll: List.Buffer[_]): coll.type =
    doRecord(fn, coll.size)
    coll

  inline def trackTime[T](fn: String)(inline op: T): T =
    if (enabled) doTrackTime(fn)(op) else op

  def doTrackTime[T](fn: String)(op: => T): T = {
    if (monitored) {
      val start = System.nanoTime
      try op finally record(fn, ((System.nanoTime - start) / 1000).toInt)
    }
    else op
  }

  final val GroupChar = '/'

  /** Aggregate all counts of all keys with a common prefix, followed by `:` */
  private def aggregate(): Unit = {
    val groups = hits.keys
      .filter(_.contains(GroupChar))
      .groupBy(_.takeWhile(_ != GroupChar))
    for ((prefix, names) <- groups; name <- names)
      hits(s"Total $prefix") += hits(name)
  }

  def maybeMonitored[T](op: => T)(using Context): T =
    if (ctx.settings.YdetailedStats.value) {
      monitored = true
      try op
      finally {
        aggregate()
        println()
        println(hits.tolist.sortBy(_._2).map{ case (x, y) => s"$x -> $y" } mkString "\n")
      }
    }
    else op
}
