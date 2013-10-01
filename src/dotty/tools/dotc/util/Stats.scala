package dotty.tools.dotc
package util

import core.Contexts._

object Stats {

  final val enabled = true

  /** The period in ms in which stack snapshots are displayed */
  final val HeartBeatPeriod = 250

  @volatile private var stack: List[String] = Nil

  def track[T](fn: String)(op: => T) = {
    stack = fn :: stack
    try op
    finally stack = stack.tail
  }

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

    override final def run() {
      Thread.sleep(HeartBeatPeriod)
      printStack(stack)
      if (continue) run()
    }
  }

  def monitorHeartBeat[T](op: => T)(implicit ctx: Context) = {
    if (ctx.settings.Yheartbeat.value) {
      var hb = new HeartBeat()
      hb.start()
      try op
      finally hb.continue = false
    } else op
  }
}