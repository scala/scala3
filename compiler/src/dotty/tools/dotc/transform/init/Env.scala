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

import scala.collection.mutable.Map

import Effects._, Potentials._, Summary._

implicit def theCtx(implicit env: Env): Context = env.ctx

case class Env(ctx: Context, summaryCache: Map[Symbol, Summary]) {
  private implicit def self: Env = this

  def withCtx(newCtx: Context): Env = this.copy(ctx = newCtx)

  def cacheFor(symbol: Symbol, summary: Summary): Unit = {
    summaryCache(symbol) = summary
  }

  /** Summary of a method or field */
  def summaryOf(symbol: Symbol): Summary =
    if (summaryCache.contains(symbol)) summaryCache(symbol)
    else trace("summary for " + symbol.show, init, s => Summary.show(s.asInstanceOf[Summary])) {
      val summary =
        if (symbol.isConstructor)
          Summarization.analyzeConstructor(symbol)
        else if (symbol.is(Flags.Method))
          Summarization.analyzeMethod(symbol)
        else // field
          Summarization.analyzeField(symbol)

      summaryCache(symbol) = summary
      summary
    }

  def effectsOf(symbol: Symbol): Effects = summaryOf(symbol)._2

  def potentialsOf(symbol: Symbol): Potentials = summaryOf(symbol)._1
}
