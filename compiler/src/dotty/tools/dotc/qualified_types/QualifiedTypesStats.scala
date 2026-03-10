package dotty.tools.dotc.qualified_types

import scala.collection.mutable.ArrayBuffer

final class QualifiedTypesStats(val enabled: Boolean):
  private val events: ArrayBuffer[(String, Long)] = ArrayBuffer.empty

  inline def record[T](event: String)(inline op: => T): T =
    if enabled then
      val start = System.nanoTime()
      try op finally events += ((event, System.nanoTime() - start))
    else op

  private def nsToMs(ns: Long): Double = ns / 1_000_000.0

  def show(): String =
    if !enabled || events.isEmpty then ""
    else
      val grouped = events.groupBy(_._1).map: (name, entries) =>
        val times = entries.map(_._2).sorted
        val count = times.size
        val totalNs = times.sum
        val minNs = times.head
        val maxNs = times.last
        val medianNs =
          if count % 2 == 1 then times(count / 2)
          else (times(count / 2 - 1) + times(count / 2)) / 2
        (name, count, totalNs, minNs, maxNs, medianNs)
      val sb = StringBuilder()
      sb ++= "Qualified types stats:\n"
      sb ++= f"  ${"event"}%-40s ${"count"}%8s ${"total ms"}%10s ${"min ms"}%10s ${"median ms"}%10s ${"max ms"}%10s\n"
      for (name, count, totalNs, minNs, maxNs, medianNs) <- grouped.toList.sortBy(-_._3) do
        sb ++= f"  $name%-40s $count%8d ${nsToMs(totalNs)}%10.0f ${nsToMs(minNs)}%10.3f ${nsToMs(medianNs)}%10.3f ${nsToMs(maxNs)}%10.3f\n"
      sb.result()
