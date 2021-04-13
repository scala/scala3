package dotty.tools.dotc
package transform
package init

import core._
import Contexts._
import Types._
import Symbols._
import Decorators._

import ast.Trees._
import ast.tpd

import reporting.trace
import config.Printers.init

import scala.collection.mutable

import Effects._, Potentials._, Summary._

class Cache {
  /** Summary of a class */
  private val summaryCache = mutable.Map.empty[ClassSymbol, ClassSummary]
  def summaryOf(cls: ClassSymbol)(using Env): ClassSummary =
    if (summaryCache.contains(cls)) summaryCache(cls)
    else trace("summary for " + cls.show, init, s => s.asInstanceOf[ClassSummary].show) {
      val summary = Summarization.classSummary(cls)
      summaryCache(cls) = summary
      summary
    }

  /** Cache for outer this */
  private case class OuterKey(warm: Warm, cls: ClassSymbol)
  private val outerCache: mutable.Map[OuterKey, Potentials] = mutable.Map.empty
  def resolveOuter(warm: Warm, cls: ClassSymbol)(using Env): Potentials =
    val key = OuterKey(warm, cls)
    if (outerCache.contains(key)) outerCache(key)
    else {
      val pots = Potentials.resolveOuter(warm.classSymbol, warm.outer.toPots, cls)
      outerCache(key) = pots
      pots
    }
}
