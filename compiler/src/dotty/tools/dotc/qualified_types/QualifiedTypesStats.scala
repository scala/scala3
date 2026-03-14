package dotty.tools.dotc.qualified_types

import scala.collection.mutable.ArrayBuffer

final class QualifiedTypesStats(val enabled: Boolean):
  private val events: ArrayBuffer[(String, (Long, String))] = ArrayBuffer.empty

  inline def record[T](event: String, inline msg: => String = "")(inline op: => T): T =
    if enabled then
      val start = System.nanoTime()
      try op
      finally
        events += ((event, (System.nanoTime() - start, msg)))
    else op

  private def nsToMs(ns: Long): Double = ns / 1_000_000.0

  def show(): String =
    if !enabled || events.isEmpty then ""
    else
      val grouped = events.groupBy(_._1).map: (name, entries) =>
        val sortedEntries = entries.map(_._2).sorted
        val count = entries.size
        val times = sortedEntries.map(_._1)
        val totalNs = times.sum
        val minNs = times.head
        val maxNs = times.last
        val maxMsg = sortedEntries.maxBy(_._1)._2
        def percentile(p: Int): Long =
          val idx = math.min((count.toLong * p / 100).toInt, count - 1)
          times(idx)
        val medianNs = percentile(50)
        val p75Ns = percentile(75)
        val p99Ns = percentile(99)
        (name, count, totalNs, minNs, medianNs, p75Ns, p99Ns, maxNs, maxMsg)
      val sb = StringBuilder()
      sb ++= "# Qualified types stats\n"
      sb ++= f"${"event"}%-40s ${"count"}%8s ${"total ms"}%10s ${"min ms"}%10s ${"median ms"}%10s ${"p75 ms"}%10s ${"p99 ms"}%10s ${"max ms"}%10s  ${"max msg"}%s\n"
      for (name, count, totalNs, minNs, medianNs, p75Ns, p99Ns, maxNs, maxMsg) <- grouped.toList.sortBy(-_._3) do
        sb ++= f"$name%-40s $count%8d ${nsToMs(totalNs)}%10.0f ${nsToMs(minNs)}%10.3f ${nsToMs(medianNs)}%10.3f ${nsToMs(p75Ns)}%10.3f ${nsToMs(p99Ns)}%10.3f ${nsToMs(maxNs)}%10.3f  $maxMsg\n"
      sb.result()
