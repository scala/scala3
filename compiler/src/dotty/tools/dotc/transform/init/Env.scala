package dotty.tools.dotc
package transform
package init

import core._
import ast.Trees._
import ast.tpd
import Contexts.Context
import Symbols._
import reporting.trace
import config.Printers.init

import scala.collection.mutable

import Effects._, Potentials._, Summary._

implicit def theCtx(implicit env: Env): Context = env.ctx

case class Env(ctx: Context, summaryCache: mutable.Map[ClassSymbol, ClassSummary]) {
  private implicit def self: Env = this

  def withCtx(newCtx: Context): Env = this.copy(ctx = newCtx)

  /** Summary of a method or field */
  def summaryOf(cls: ClassSymbol): ClassSummary =
    if (summaryCache.contains(cls)) summaryCache(cls)
    else trace("summary for " + cls.show, init, s => s.asInstanceOf[ClassSummary].show) {
      val summary = Summarization.classSummary(cls)
      summaryCache(symbol) = summary
      summary
    }
}
