package dotty.tools
package dotc
package util

import scala.annotation.internal.sharable

import core.Contexts._
import collection.mutable

@sharable object Stats {

  final val enabled = false

  /** The period in ms in which stack snapshots are displayed */
  final val HeartBeatPeriod = 250

  var monitored: Boolean = false

  @volatile private[this] var stack: List[String] = Nil

  val hits: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int] {
    override def default(key: String): Int = 0
  }

  @forceInline
  def record(fn: => String, n: => Int = 1): Unit =
    if (enabled) doRecord(fn, n)

  def doRecord(fn: String, n: Int) =
    if (monitored) {
      val name = if (fn.startsWith("member-")) "member" else fn
      hits(name) += n
    }

  @forceInline
  def track[T](fn: String)(op: => T): T =
    if (enabled) doTrack(fn)(op) else op

  def doTrack[T](fn: String)(op: => T): T =
    if (monitored) {
      stack = fn :: stack
      record(fn)
      try op
      finally stack = stack.tail
    } else op

  @forceInline
  def trackTime[T](fn: String)(op: => T): T =
    if (enabled) doTrackTime(fn)(op) else op

  def doTrackTime[T](fn: String)(op: => T): T = {
    def op1 = op
    if (monitored) {
      val start = System.nanoTime
      try op1 finally record(fn, ((System.nanoTime - start) / 1000).toInt)
    } else op1
  }

  class HeartBeat extends Thread() {
    @volatile private[Stats] var continue: Boolean = true

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

  def maybeMonitored[T](op: => T)(implicit ctx: Context): T = {
    if (ctx.settings.YdetailedStats.value) {
      val hb = new HeartBeat()
      if (ctx.settings.Yheartbeat.value) hb.start()
      monitored = true
      try op
      finally {
        hb.continue = false
        println()
        println(hits.toList.sortBy(_._2).map{ case (x, y) => s"$x -> $y" } mkString "\n")
        println(s"uniqueInfo (size, accesses, collisions): ${ctx.base.uniquesSizes}")
      }
    } else op
  }
}
