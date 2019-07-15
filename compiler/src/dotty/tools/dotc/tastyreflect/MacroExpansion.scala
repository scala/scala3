package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.util.{Property, SourcePosition, Spans}

object MacroExpansion {

  private val MacroExpansionPosition = new Property.Key[SourcePosition]

  def position(implicit ctx: Context): Option[SourcePosition] =
    ctx.property(MacroExpansionPosition)

  def context(inlinedFrom: tpd.Tree)(implicit ctx: Context): Context =
    ctx.fresh.setProperty(MacroExpansionPosition, SourcePosition(inlinedFrom.source, inlinedFrom.span)).withSource(inlinedFrom.source)

}
