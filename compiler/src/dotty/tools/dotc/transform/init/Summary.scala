package dotty.tools.dotc
package transform
package init

import scala.collection.mutable

import core._
import Contexts.Context
import reporting.trace
import config.Printers.init

import Potentials._, Effects._

object Summary {
  type Summary = (Potentials, Effects)
  val empty: Summary = (Potentials.empty, Effects.empty)

  case class ClassSummary(
    outer: Potentials,
    cls: ClassSymbol,
    parents: List[Potentials]
  ) {
    private val summaryCache: mutable.Map[ClassSymbol, ClassSummary] = mutable.Map.empty

    def summaryOf(member: Symbol): Summary =
      if (summaryCache.contains(member)) summaryCache(clmembers)
      else trace("summary for " + member.show, init, s => Summary.show(s.asInstanceOf[Summary])) {
        val summary =
          if (symbol.isConstructor)
            Summarization.analyzeConstructor(symbol) // TODO: asSeenFrom
          else if (symbol.is(Flags.Method))
            Summarization.analyzeMethod(symbol)      // TODO: asSeenFrom
          else // field
            Summarization.analyzeField(symbol)       // TODO: asSeenFrom

        summaryCache(symbol) = summary
        summary
      }
  }

  def show(summary: Summary)(implicit ctx: Context): String = {
    val pots = Potentials.show(summary._1)
    val effs = Effects.show(summary._2)
    s"([$pots], [$effs])"
  }

  def (summary1: Summary) union (summary2: Summary): Summary =
    (summary1._1 ++ summary2._1, summary1._2 ++ summary2._2)

  def (summary: Summary) + (pot: Potential): Summary =
    (summary._1 + pot, summary._2)

  def (summary: Summary) + (eff: Effect): Summary =
    (summary._1, summary._2 + eff)

  def (summary: Summary) withPots (pots: Potentials): Summary =
    (summary._1 ++ pots, summary._2)

  def (summary: Summary) withEffs (effs: Effects): Summary =
    (summary._1, summary._2 ++ effs)
}
