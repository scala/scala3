package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.typer.Typer
import dotty.tools.dotc.util.{Property, SourcePosition}

object MacroExpansion {

  private val MacroExpansionPosition = new Property.Key[SourcePosition]

  def position(using Context): Option[SourcePosition] =
    ctx.property(MacroExpansionPosition)

  def context(inlinedFrom: tpd.Tree)(using Context): Context =
    QuotesCache.init(ctx.fresh).setProperty(MacroExpansionPosition, SourcePosition(inlinedFrom.source, inlinedFrom.span)).setTypeAssigner(new Typer(ctx.nestingLevel + 1)).withSource(inlinedFrom.source)
}

