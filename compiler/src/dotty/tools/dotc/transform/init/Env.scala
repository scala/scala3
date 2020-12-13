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

implicit def theCtx(implicit env: Env): Context = env.ctx

case class Env(ctx: Context) {
  private implicit def self: Env = this

  // Methods that should be ignored in the checking
  lazy val ignoredMethods: Set[Symbol] = Set(
    defn.Any_getClass,
    defn.Any_isInstanceOf,
    defn.Object_eq,
    defn.Object_ne,
    defn.Object_synchronized
  )

  def withCtx(newCtx: Context): Env = this.copy(ctx = newCtx)

  def withOwner(owner: Symbol) = this.copy(ctx = this.ctx.withOwner(owner))

  /** Whether values of a given type is always fully initialized?
   *
   *  It's true for primitive values
   */
  def isAlwaysInitialized(tp: Type)(implicit env: Env): Boolean = {
    val sym = tp.widen.finalResultType.typeSymbol
    sym.isPrimitiveValueClass || sym == defn.StringClass
  }

  /** Summary of a class */
  private val summaryCache = mutable.Map.empty[ClassSymbol, ClassSummary]
  def summaryOf(cls: ClassSymbol): ClassSummary =
    if (summaryCache.contains(cls)) summaryCache(cls)
    else trace("summary for " + cls.show, init, s => s.asInstanceOf[ClassSummary].show) {
      val summary = Summarization.classSummary(cls)
      summaryCache(cls) = summary
      summary
    }

  /** Cache for outer this */
  private case class OuterKey(warm: Warm, cls: ClassSymbol)
  private val outerCache: mutable.Map[OuterKey, Potentials] = mutable.Map.empty
  def resolveOuter(warm: Warm, cls: ClassSymbol)(implicit env: Env): Potentials =
    val key = OuterKey(warm, cls)
    if (outerCache.contains(key)) outerCache(key)
    else {
      val pots = Potentials.resolveOuter(warm.classSymbol, warm.outer.toPots, cls)
      outerCache(key) = pots
      pots
    }
}
