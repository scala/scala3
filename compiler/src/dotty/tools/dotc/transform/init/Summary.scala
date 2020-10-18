package dotty.tools
package dotc
package transform
package init

import scala.collection.mutable

import core._
import Contexts._
import Symbols._
import reporting.trace
import config.Printers.init

import Potentials._, Effects._, Util._

object Summary {
  type Summary = (Potentials, Effects)
  val empty: Summary = (Potentials.empty, Effects.empty)

  /** Summary of class.
   *
   *  It makes ObjectPart construction easier with already established raw outer for parents.
   */
  case class ClassSummary(currentClass: ClassSymbol, parentOuter: Map[ClassSymbol, Potentials]) {
    private val summaryCache: mutable.Map[Symbol, Summary] = mutable.Map.empty

    def cacheFor(member: Symbol, summary: Summary)(using Context): Unit = {
      traceIndented("cache for " + member.show + ", summary = " + Summary.show(summary), init)
      assert(member.owner == currentClass, "owner = " + member.owner.show + ", current = " + currentClass.show)
      summaryCache(member) = summary
    }

    def summaryOf(member: Symbol)(implicit env: Env): Summary =
      if (summaryCache.contains(member)) summaryCache(member)
      else trace("summary for " + member.show, init, s => Summary.show(s.asInstanceOf[Summary])) {
        implicit val env2 = env.withOwner(member)
        val summary =
          if (member.isConstructor)
            Summarization.analyzeConstructor(member)
          else if (member.is(Flags.Method))
            Summarization.analyzeMethod(member)
          else // field
            Summarization.analyzeField(member)

        summaryCache(member) = summary
        summary
      }

    def effectsOf(member: Symbol)(implicit env: Env): Effects = summaryOf(member)._2
    def potentialsOf(member: Symbol)(implicit env: Env): Potentials = summaryOf(member)._1

    def show(using Context): String =
      "ClassSummary(" + currentClass.name.show +
        ", parents = " + parentOuter.map { case (k, v) => k.show + "->" + "[" + Potentials.show(v) + "]" }
  }

  def show(summary: Summary)(using Context): String = {
    val pots = Potentials.show(summary._1)
    val effs = Effects.show(summary._2)
    s"([$pots], [$effs])"
  }

  extension (summary1: Summary) def union (summary2: Summary): Summary =
    (summary1._1 ++ summary2._1, summary1._2 ++ summary2._2)

  extension (summary: Summary) def + (pot: Potential): Summary =
    (summary._1 + pot, summary._2)

  extension (summary: Summary) def + (eff: Effect): Summary =
    (summary._1, summary._2 + eff)

  extension (summary: Summary) def withPots (pots: Potentials): Summary =
    (summary._1 ++ pots, summary._2)

  extension (summary: Summary) def withEffs (effs: Effects): Summary =
    (summary._1, summary._2 ++ effs)
}
