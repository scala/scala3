package dotty.tools
package dottydoc
package core

import dotc.core.Symbols._
import dotc.core.Flags._
import dotc.core.Decorators._
import dotc.core.Comments.ContextDocstrings
import model.{ Package, Entity }
import model.comment.Comment

import dotc.core.Contexts.Context
import dotc.printing.Highlighting._
import dotc.printing.Formatting.hl
import dotc.util.{ SourcePosition, NoSourcePosition }

class ContextDottydoc extends ContextDocstrings {
  import scala.collection.mutable

  private[this] val _packages: mutable.Map[String, Package] = mutable.Map.empty
  def packages: Map[String, Package] = _packages.toMap
  def packagesMutable: mutable.Map[String, Package] = _packages

  private[this]  var _statistics: Map[String, Statistics] = Map.empty
  def registerStatistics(pkgName: String, stat: Statistics): Unit =
    _statistics = _statistics + (pkgName -> stat)

  def statistics: Map[String, Statistics] = _statistics

  /** Should perhaps factorize this into caches that get flushed */
  private[this] var _defs: Map[Symbol, Set[Symbol]] = Map.empty
  def defs(sym: Symbol): Set[Symbol] = _defs.get(sym).getOrElse(Set.empty)

  def addDef(s: Symbol, d: Symbol): Unit = _defs = (_defs + {
    s -> _defs.get(s).map(xs => xs + d).getOrElse(Set(d))
  })

  def error(msg: String, pos: SourcePosition)(implicit ctx: Context): Unit = ctx.error({
    NoColor("[") + Red("doc error") + "] " + msg
  }.toString, pos)

  def error(msg: String)(implicit ctx: Context): Unit = error(msg, NoSourcePosition)

  def warn(msg: String, pos: SourcePosition)(implicit ctx: Context): Unit = ctx.warning({
    NoColor("[") + Yellow("doc warn") + "] " + msg
  }.toString, pos)

  def warn(msg: String)(implicit ctx: Context): Unit = warn(msg, NoSourcePosition)

  def echo(msg: String, pos: SourcePosition)(implicit ctx: Context): Unit = ctx.echo({
    "[doc info] " + msg
  }.toString, pos)

  def echo(msg: String)(implicit ctx: Context): Unit = echo(msg, NoSourcePosition)

  def debug(msg: String, pos: SourcePosition)(implicit ctx: Context): Unit =
    if (ctx.settings.Ydebug.value) ctx.inform({
      "[doc debug] " + msg
    }.toString, pos)

  def debug(msg: String)(implicit ctx: Context): Unit = debug(msg, NoSourcePosition)

  def printSummary()(implicit ctx: Context): Unit = {
    def colored(part: Int, total: Int) =
      if (total == 0) "0"
      else {
        val percentage = (part * 100.0 / total).toInt
        val str = s"$part/$total ($percentage%)"

        if (percentage > 75) Green(str)
        else if (percentage > 50) Yellow(str)
        else Red(str)
      }

    val totalEntities = statistics.totalEntities

    val projectName = ctx.settings.projectName.value
    val warningsText =
      if (ctx.reporter.hasWarnings)
        s"total warnings with regards to compilation and documentation: ${ctx.reporter.warningCount}"
      else ""

    val api = statistics.values.iterator.map(_.api).foldLeft(Counters(0,0,0,0,0,0))(_ merge _)
    val internalApi = statistics.values.iterator.map(_.internalApi).foldLeft(Counters(0,0,0,0,0,0))(_ merge _)

    val apiSummary = (for {
      (pkgName, stat) <- statistics.toList.sortBy(_._1)
    } yield {
      val pub = colored(stat.api.publicDocstrings, stat.api.publicEntities)
      val pro = colored(stat.api.protectedDocstrings, stat.api.protectedEntities)
      s"""|package $pkgName
          |${Blue("-" * ctx.settings.pageWidth.value)}
          |public: $pub \t protected: $pro
          |""".stripMargin
    }).mkString("\n")

    val internalSummary = (for {
      (pkgName, stat) <- statistics.toList.sortBy(_._1)
    } yield {
      val pub = colored(stat.internalApi.publicDocstrings, stat.internalApi.publicEntities)
      val pro = colored(stat.internalApi.protectedDocstrings, stat.internalApi.protectedEntities)
      val pri = colored(stat.internalApi.privateDocstrings, stat.internalApi.privateEntities)
      s"""|package $pkgName
          |${Blue("-" * ctx.settings.pageWidth.value)}
          |public: $pub \t protected: $pro \t private: $pri
          |""".stripMargin
    }).mkString("\n")

    ctx.echo {
      s"""|${Blue("=" * ctx.settings.pageWidth.value)}
          |Dottydoc summary report for project `$projectName`
          |${Blue("=" * ctx.settings.pageWidth.value)}
          |Documented members in public API:
          |
          |$apiSummary
          |
          |Summary:
          |
          |public members with docstrings:    ${colored(api.publicDocstrings, api.publicEntities)}
          |${hl("protected")} members with docstrings: ${colored(api.protectedDocstrings, api.protectedEntities)}
          |${Blue("=" * ctx.settings.pageWidth.value)}
          |
          |Documented members in internal API:
          |
          |$internalSummary
          |
          |Summary internal API:
          |
          |public members with docstrings:    ${colored(internalApi.publicDocstrings, internalApi.publicEntities)}
          |${hl("protected")} members with docstrings: ${colored(internalApi.protectedDocstrings, internalApi.protectedEntities)}
          |${hl("private")} members with docstrings:   ${colored(internalApi.privateDocstrings, internalApi.privateEntities)}
          |$warningsText""".stripMargin
    }
  }
}
