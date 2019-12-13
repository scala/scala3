package dotty.tools.dotc
package transform
package init

import scala.collection.mutable

import core._
import Contexts.Context
import Symbols._
import reporting.trace
import config.Printers.init

import Potentials._, Effects._

object Summary {
  type Summary = (Potentials, Effects)
  val empty: Summary = (Potentials.empty, Effects.empty)

  /** Summary of class.
   *
   *  It makes ObjectPart construction easier with already established raw outer for parents.
   */
  case class ClassSummary(currentClass: ClassSymbol, parentOuter: Map[ClassSymbol, Potentials]) {
    private val summaryCache: mutable.Map[Symbol, Summary] = mutable.Map.empty

    def cacheFor(member: Symbol, summary: Summary): Unit = {
      assert(member.owner == currentClass, "owner = " + member.owner.show + ", current = " + currentClass.show)
      summaryCache(member) = summary
    }

    def summaryOf(member: Symbol)(implicit ctx: Context): Summary =
      if (summaryCache.contains(member)) summaryCache(member)
      else trace("summary for " + member.show, init, s => Summary.show(s.asInstanceOf[Summary])) {
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

    def effectsOf(member: Symbol)(implicit ctx: Context): Effects = summaryOf(member)._2
    def potentialsOf(member: Symbol)(implicit ctx: Context): Potentials = summaryOf(member)._1

    def show(implicit ctx: Context): String =
      "ObjectPart(" + currentClass.name.show +
        "parents = {" + parentOuter.map { case (k, v) => k.show + "->" + "[" + Potentials.show(v) + "]" } + "}"
  }

  /** Part of object.
   *
   *  It makes prefix and outer substitution easier in effect checking.
   */
  case class ObjectPart(
    thisValue: Warm | ThisRef,                  // the potential for `this`, it can be Warm or ThisRef
    currentClass: ClassSymbol,                  // current class
    currentOuter: Potentials,                   // the immediate outer for current class, empty for local and top-level classes
    parentOuter: Map[ClassSymbol, Potentials]   // outers for direct parents
  ) {
    private val summaryCache: mutable.Map[Symbol, Summary] = mutable.Map.empty

    /** Summary of a member field or method, with `this` and outers substituted */
    def summaryOf(member: Symbol)(implicit ctx: Context): Summary = ???

    def show(implicit ctx: Context): String =
      "ObjectPart(this = " + thisValue.show + ","  + currentClass.name.show + ", outer = " + Potentials.show(currentOuter) +
        "parents = {" + parentOuter.map { case (k, v) => k.show + "->" + "[" + Potentials.show(v) + "]" } + "}"
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
