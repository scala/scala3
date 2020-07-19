package dottyBench.tools.dotc.tastyreflect

import dottyBench.tools.dotc.ast.tpd
import dottyBench.tools.dotc.core._
import dottyBench.tools.dotc.core.Contexts._
import dottyBench.tools.dotc.typer.Typer
import dottyBench.tools.dotc.util.{Property, SourcePosition, Spans}

object MacroExpansion {

  private val MacroExpansionPosition = new Property.Key[SourcePosition]

  def position(using Context): Option[SourcePosition] =
    ctx.property(MacroExpansionPosition)

  def context(inlinedFrom: tpd.Tree)(using c: Context): Context =
    c.fresh
      .setProperty(MacroExpansionPosition, SourcePosition(inlinedFrom.source, inlinedFrom.span))
      .setTypeAssigner(new Typer)
      .withSource(inlinedFrom.source)
      .toContext(using c.cstate)
}

