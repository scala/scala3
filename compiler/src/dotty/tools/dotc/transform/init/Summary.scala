package dotty.tools.dotc
package transform
package init

import scala.collection.mutable
import scala.annotation.targetName

import core._
import Contexts._
import Symbols._
import reporting.trace
import config.Printers.init

import Potentials._, Effects._, Util._

case class Summary(pots: Potentials, effs: Effects) {
  def +(summary2: Summary): Summary =
    Summary(pots ++ summary2.pots, this.effs ++ summary2.effs)

  def +(pot: Potential): Summary =
    Summary(pots :+ pot, effs)

  def +(eff: Effect): Summary =
    Summary(pots, effs :+ eff)

  def dropPotentials: Summary =
    Summary(Potentials.empty, effs)

  @targetName("withPotentials")
  def ++(pots: Potentials): Summary =
    Summary(this.pots ++ pots, effs)

  @targetName("withEffects")
  def ++(effs: Effects): Summary =
    Summary(pots, this.effs ++ effs)

  def show(using Context): String = {
    val pots = Potentials.show(this.pots)
    val effs = Effects.show(this.effs)
    s"([$pots], [$effs])"
  }
}

object Summary {
  val empty: Summary = Summary(Potentials.empty, Effects.empty)

  def apply(pots: Potentials): Summary = empty ++ pots

  @targetName("withEffects")
  def apply(effs: Effects): Summary = empty ++ effs

  def apply(pot: Potential): Summary = empty + pot

  def apply(eff: Effect): Summary = empty + eff
}

/** Summary of class.
  *
  *  It makes ObjectPart construction easier with already established raw outer for parents.
  */
case class ClassSummary(currentClass: ClassSymbol, parentOuter: Map[ClassSymbol, Potentials]) {
  private val summaryCache: mutable.Map[Symbol, Summary] = mutable.Map.empty

  def cacheFor(member: Symbol, summary: Summary)(using Context): Unit = {
    traceIndented("cache for " + member.show + ", summary = " + summary.show, init)
    assert(member.owner == currentClass, "owner = " + member.owner.show + ", current = " + currentClass.show)
    summaryCache(member) = summary
  }

  def summaryOf(member: Symbol)(implicit env: Env): Summary =
    if (summaryCache.contains(member)) summaryCache(member)
    else trace("summary for " + member.show, init, s => s.asInstanceOf[Summary].show) {
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

  def effectsOf(member: Symbol)(implicit env: Env): Effects = summaryOf(member).effs
  def potentialsOf(member: Symbol)(implicit env: Env): Potentials = summaryOf(member).pots

  def show(using Context): String =
    "ClassSummary(" + currentClass.name.show +
      ", parents = " + parentOuter.map { case (k, v) => k.show + "->" + "[" + Potentials.show(v) + "]" }
}
