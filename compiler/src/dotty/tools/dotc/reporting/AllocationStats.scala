package dotty.tools.dotc.reporting

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import java.lang.ref.WeakReference
import java.util.concurrent.atomic.{AtomicInteger, AtomicReferenceArray}

import dotty.tools.dotc.core.Denotations.{Denotation, MultiDenotation, SingleDenotation}
import dotty.tools.dotc.core.Periods
import dotty.tools.dotc.core.Periods.Period
import dotty.tools.dotc.typer.FrontEnd
import dotty.tools.sharable

import scala.collection.concurrent.TrieMap


/** Collect collection allocation statistics in the entire compiler.
 * Threads safe but makes compiler slow
 * @author Dmitry Petrahsko
 */
object AllocationStats {
  final val collect = true

  @sharable private val counts = collection.concurrent.TrieMap[(Class[_], Class[_]), Int]()
  @sharable private val lastPhase = new ThreadLocal[Class[_]]()
  @sharable private val denotations = new TrieMap[SingleDenotation, SingleDenotation]()

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

  @inline def registerAllocation[T](obj: T)(implicit ctx: Context): T  = {
    // this method should be kept very small
    if (collect)
      registerAllocation(ctx, obj)

    obj
  }


  private def registerAllocation[T](ctx: Context, obj: T): T = {
    val p: Class[_] = fixPhase(if (ctx eq null) null else ctx.phase.getClass)
    //markReported(obj)
    val key = (if (p eq null) null else p, obj.getClass)

    obj match {
      case t: SingleDenotation =>
        denotations.put(t, t)
      case _ =>
    }

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

  def report()(implicit ctx: Context): String = {
    val data = counts.readOnlySnapshot()
    clear()
    val longestNameLength = data.keys.map(x => x._2.getName.length).max
    val total = data.groupBy( x => x._1._2).map(x => (x._1, x._2.values.sum)).toSeq.sortBy(-_._2)
    val byPhase = data.groupBy(x => x._1._1).map(x => (x._1, x._2.map(x => (x._1._2, x._2)))).toSeq.sortBy(x => -x._2.map(_._2).sum)
    val denotStarts = denotations.keySet.map(_.initial).filter(x => x.validFor != Periods.Nowhere).toSeq
    val denotationsByLength = denotStarts.groupBy(x => x.history.length).toSeq.sortBy(x => -x._1)

    def creatorPhase(p: Period): Phase = {
      val x = ctx.withPhase(p.firstPhaseId).phase
      if (x.isTyper) x
      else x.prev // all phases by typer create period.next and run at it
    }
    val newDenotationsPerPhase =
      denotStarts.flatMap(x => x.history).groupBy(x => creatorPhase(x.validFor).phaseName)
        .toSeq.sortBy(x => -x._2.length)

    def pad(o: AnyRef) =
      o.toString.padTo(longestNameLength, " ").mkString

    "Total allocations:\n" + total.map{ x =>
         "   " + pad(x._1.getName) + " -> " + x._2
    }.mkString("\n") +
      "\n\n\nClass allocations by phase:\n" + byPhase.map { x =>
      val phaseName = x._1.getSimpleName
      val subtrees = x._2.toSeq.sortBy(-_._2).map(x => s"${pad(x._1.getName)} -> ${x._2}").mkString("\n   ", "\n   ","\n")
      phaseName ++ subtrees
    }.mkString("\n") + {
      "\n\n\nDenotation length distribution:\n" + denotationsByLength.map {
        x => s"${pad(x._1.toString)} -> ${x._2.size}"
      }.mkString("\n")
    } + {
      "\n\n\nNew denotations retained per phase:\n" + newDenotationsPerPhase.map{
        x => s"${pad(x._1)} -> ${x._2.size}"
      }.mkString("\n")
    }
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
