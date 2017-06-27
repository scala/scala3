package dotty.tools.dotc.reporting

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import java.lang.ref.WeakReference
import java.util.concurrent.atomic.{AtomicInteger, AtomicReferenceArray}

import dotty.tools.dotc.typer.FrontEnd


/** Collect collection allocation statistics in the entire compiler.
 * Threads safe but makes compiler slow
 * @author Dmitry Petrahsko
 */
object AllocationStats {
  final val collect = false

  private val counts = collection.concurrent.TrieMap[(Class[_], Class[_]), Int]()
  private val lastPhase = new ThreadLocal[Class[_]]()

  def fixPhase(x: Class[_]): Class[_] = {
    if (x eq null) {
      val r = lastPhase.get()
      if (r ne null) r
      else classOf[FrontEnd]
    } else {
      lastPhase.set(x)
      x
    }
  }

  def registerAllocation[T](obj: T)(implicit ctx: Context): T  = {
    if (collect)
      registerAllocation(fixPhase(if (ctx eq null) null else ctx.phase.getClass), obj)

    obj
  }


  private def registerAllocation[T](p: Class[_], obj: T): T = {
    //markReported(obj)
    val key = (if (p eq null) null else p, obj.getClass)

    counts.putIfAbsent(key, 0)
    var repeat = true
    while (repeat) {
      val old = counts(key)
      repeat = !counts.replace(key, old, old + 1)
    }

    obj
  }

  def clear() = {
    counts.clear()
  }

  def report(): String = {
    val data = counts.readOnlySnapshot()
    clear()
    val longestNameLength = data.keys.map(x => x._2.getName.length).max
    val byPhase = data.groupBy(x => x._1._1).map(x => (x._1, x._2.map(x => (x._1._2, x._2)))).toSeq.sortBy(x => -x._2.map(_._2).sum)

    "Class allocations by phase:\n" + byPhase.map { x =>
      val phaseName = x._1.getSimpleName
      val subtrees = x._2.toSeq.sortBy(-_._2).map(x => s"${x._1.getName.padTo(longestNameLength, " ").mkString} -> ${x._2}").mkString("\n   ", "\n   ","\n")
      phaseName ++ subtrees
    }.mkString("\n")
  }

  /*
  final val historySize = 1024
  private val last = new AtomicReferenceArray[WeakReference[Any]](historySize)
  private val rec = new AtomicInteger(0)

  def checkReported(obj: Object): Unit = {
    var i = 0
    while (i < historySize) {
      val s = last.get(i)
      if ((s ne null) && s.get() == obj) return;
      i = i + 1
    }
    throw new RuntimeException(s"object not registered in allocation statistics: ${obj.getClass}")
  }

  def markReported(obj: Any): Unit = {
    val my = rec.getAndAdd(1)
    val idx = ((my % historySize) + historySize) % historySize
    last.set(idx, new WeakReference[Any](obj))
  }*/

}
