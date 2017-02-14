package dotty.tools
package dottydoc
package core

import dotc.core.Phases.Phase
import dotc.core.Contexts.Context
import dotc.core.Symbols.Symbol
import dotc.core.Decorators._
import dotc.core.Flags._
import dotc.CompilationUnit
import dottydoc.util.syntax._
import dottydoc.util.traversing._

import model._

object Statistics {
  implicit class MapTotals(val map: Map[String, Statistics]) extends AnyVal {
    def totalEntities =
      map.values.foldLeft(0)(_ + _.totalEntities)
  }
}

case class Statistics(pkgName: String, api: Counters, internalApi: Counters) {
  def totalEntities =
    api.totalEntities + internalApi.totalEntities

  def totalDocstrings =
    api.totalDocstrings + internalApi.totalDocstrings
}

case class Counters(
  publicEntities: Int,
  privateEntities: Int,
  protectedEntities: Int,

  publicDocstrings: Int,
  privateDocstrings: Int,
  protectedDocstrings: Int
) {
  def totalEntities =
    publicEntities + privateEntities + protectedEntities

  def totalDocstrings =
    publicDocstrings + privateDocstrings + protectedDocstrings

  def merge(o: Counters): Counters = Counters(
    publicEntities + o.publicEntities,
    privateEntities + o.privateEntities,
    protectedEntities + o.protectedEntities,
    publicDocstrings + o.publicDocstrings,
    privateDocstrings + o.privateDocstrings,
    protectedDocstrings + o.protectedDocstrings
  )
}

class StatisticsPhase extends Phase {

  def phaseName = "StatisticsPhase"

  override def run(implicit ctx: Context): Unit = ()

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    for {
      (pkgName, pack) <- ctx.docbase.packages
      externalApi = collectPublicStats(pack)
      internalApi = collectInternalStats(pack)
      stats = Statistics(pkgName, externalApi, internalApi)
    } ctx.docbase.registerStatistics(pkgName, stats)

    units
  }

  def collectPublicStats(pack: Package)(implicit ctx: Context): Counters = {
    var publicEntities: Int = 0
    var protectedEntities: Int = 0
    var publicDocstrings: Int = 0
    var protectedDocstrings: Int = 0

    if (pack.comment.isDefined) {
      publicEntities += 1
      publicDocstrings += 1
    }

    def doCount(sym: Symbol, comment: Int): Unit =
      if (!sym.is(Protected)) {
        publicEntities += 1
        publicDocstrings += comment
      }
      else {
        protectedEntities += 1
        protectedDocstrings += comment
      }


    def recur(e: Entity, reachable: Boolean): Unit = {
      val isVisible = !e.symbol.is(Private) && !e.symbol.privateWithin.exists
      val shouldCount = isVisible && reachable
      e match {
        case e: Package => ()
        case e: Entity with Members  => if (shouldCount) {
          doCount(e.symbol, if (e.comment.isDefined) 1 else 0)
          e.members.foreach { c =>
            if (!(e.symbol.is(Final) && c.symbol.is(Protected))) recur(c, true)
          }
        }
        case e =>
          if (shouldCount) doCount(e.symbol, if (e.comment.isDefined) 1 else 0)
      }
    }

    pack.members.foreach(recur(_, true))
    Counters(publicEntities, 0, protectedEntities, publicDocstrings, 0, protectedDocstrings)
  }

  def collectInternalStats(pack: Package)(implicit ctx: Context): Counters = {
    var publicEntities: Int = 0
    var privateEntities: Int = 0
    var protectedEntities: Int = 0
    var publicDocstrings: Int = 0
    var privateDocstrings: Int = 0
    var protectedDocstrings: Int = 0

    def doCount(sym: Symbol, comment: Int): Unit =
      if (sym.is(Private)) {
        privateEntities += 1
        privateDocstrings += comment
      }
      else if (!sym.is(Protected)) {
        publicEntities += 1
        publicDocstrings += comment
      }
      else {
        protectedEntities += 1
        protectedDocstrings += comment
      }


    def recur(e: Entity, reachable: Boolean): Unit = {
      val internal = !reachable || e.symbol.is(Private) || e.symbol.privateWithin.exists
      e match {
        case _: Package => ()
        case e: Entity with Members =>
          e.members.foreach { c =>
            val childIsInternal = !internal || (e.symbol.is(Final) && c.symbol.is(Protected))
            recur(c, childIsInternal)
          }
          if (internal) doCount(e.symbol, if (e.comment.isDefined) 1 else 0)
        case _ =>
          if (internal) doCount(e.symbol, if (e.comment.isDefined) 1 else 0)
      }
    }

    pack.members.foreach(recur(_, true))
    Counters(publicEntities, privateEntities, protectedEntities, publicDocstrings, privateDocstrings, protectedDocstrings)
  }
}
