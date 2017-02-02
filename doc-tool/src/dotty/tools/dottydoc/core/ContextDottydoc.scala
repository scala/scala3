package dotty.tools
package dottydoc
package core

import dotc.core.Symbols.Symbol
import dotc.core.Comments.ContextDocstrings
import model.Package

import dotc.core.Contexts.Context
import dotc.printing.Highlighting._
import dotc.util.{ SourcePosition, NoSourcePosition }

class ContextDottydoc extends ContextDocstrings {
  import scala.collection.mutable

  private[this] val _packages: mutable.Map[String, Package] = mutable.Map.empty
  def packages: Map[String, Package] = _packages.toMap
  def packagesMutable: mutable.Map[String, Package] = _packages

  /** Should perhaps factorize this into caches that get flushed */
  private var _defs: Map[Symbol, Set[Symbol]] = Map.empty
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
    if (ctx.settings.debug.value) ctx.inform({
      "[doc debug] " + msg
    }.toString, pos)

  def debug(msg: String)(implicit ctx: Context): Unit = debug(msg, NoSourcePosition)
}
