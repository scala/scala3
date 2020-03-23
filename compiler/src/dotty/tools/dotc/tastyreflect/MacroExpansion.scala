package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.typer.Typer
import dotty.tools.dotc.util.{Property, SourcePosition, Spans}

object MacroExpansion {

  private val MacroExpansionPosition = new Property.Key[SourcePosition]

  def position(implicit ctx: Context): Option[SourcePosition] =
    ctx.property(MacroExpansionPosition)

  def context(inlinedFrom: tpd.Tree)(implicit ctx: Context): Context =
    val source = inlinedFrom.source
    val span = inlinedFrom.span
    val sourcePos = SourcePosition(source, span)
    assert(source.exists)
    assert(span.exists, inlinedFrom.uniqueId)
    ctx.fresh.setProperty(MacroExpansionPosition, sourcePos).setTypeAssigner(new Typer).withSource(source)
}

