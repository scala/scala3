package dotty.tools
package dotc
package util

import core.Contexts._
import collection.mutable

@sharable object Stats {

  final val enabled = false

  /** The period in ms in which stack snapshots are displayed */
  final val HeartBeatPeriod = 250

  var monitored = false

  @volatile private var stack: List[String] = Nil

  val hits = new mutable.HashMap[String, Int] {
    override def default(key: String): Int = 0
  }

  @inline
  def record(fn: String, n: Int = 1) =
    if (enabled) doRecord(fn, n)

  private def doRecord(fn: String, n: Int) =
    if (monitored) {
      val name = if (fn.startsWith("member-")) "member" else fn
      hits(name) += n
    }

  @inline
  def track[T](fn: String)(op: => T) =
    if (enabled) doTrack(fn)(op) else op

  def doTrack[T](fn: String)(op: => T) =
    if (monitored) {
      stack = fn :: stack
      record(fn)
      try op
      finally stack = stack.tail
    } else op

  @inline
  def trackTime[T](fn: String)(op: => T) =
    if (enabled) doTrackTime(fn)(op) else op

  def doTrackTime[T](fn: String)(op: => T) =
    if (monitored) {
      val start = System.nanoTime
      try op finally record(fn, ((System.nanoTime - start) / 1000).toInt)
    } else op

  class HeartBeat extends Thread() {
    @volatile private[Stats] var continue = true

    private def printStack(stack: List[String]): Unit = stack match {
      case str :: rest =>
        printStack(rest)
        print(s"-> $str ")
      case Nil =>
        println()
        print("|")
    }

    override final def run(): Unit = {
      Thread.sleep(HeartBeatPeriod)
      printStack(stack)
      if (continue) run()
    }
  }

  def maybeMonitored[T](op: => T)(implicit ctx: Context) = {
    if (ctx.settings.YdetailedStats.value) {
      val hb = new HeartBeat()
      if (ctx.settings.Yheartbeat.value) hb.start()
      monitored = true
      try op
      finally {
        hb.continue = false
        println()
        println(hits.toList.sortBy(_._2).map{ case (x, y) => s"$x -> $y" } mkString "\n")
        println(s"sizes: ${ctx.base.uniquesSizes}")
      }
    } else op
  }
}
