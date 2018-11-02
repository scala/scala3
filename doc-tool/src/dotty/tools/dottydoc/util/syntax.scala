package dotty.tools
package dottydoc
package util

import dotc.core.Contexts.ContextRenamed
import dotc.core.Comments._
import model.Package
import core.ContextDottydoc
import dotc.core.Symbols._

import dotc.util.{ SourcePosition, SourceFile }
import dotc.util.Positions.Position
import scala.io.Codec

object syntax {
  implicit class ContextWithContextDottydoc(val ctx: ContextRenamed) extends AnyVal {
    def docbase: ContextDottydoc = ctx.docCtx.getOrElse {
      throw new IllegalStateException("DocBase must be set before running dottydoc phases")
    }.asInstanceOf[ContextDottydoc]
  }

  implicit class SymbolExtensions(val sym: Symbol) extends AnyVal {
    def sourcePosition(pos: Position)(implicit ctx: ContextRenamed): SourcePosition =
      new SourceFile(sym.sourceFile, Codec(ctx.settings.encoding.value)) atPos pos

  }
}
