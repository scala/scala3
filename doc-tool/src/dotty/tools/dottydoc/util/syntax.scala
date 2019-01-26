/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package dotty.tools
package dottydoc
package util

import dotc.core.Contexts.Context
import dotc.core.Comments._
import model.Package
import core.ContextDottydoc
import dotc.core.Symbols._

import dotc.util.{ SourcePosition, SourceFile }
import dotc.util.Positions.Position
import scala.io.Codec

object syntax {
  implicit class ContextWithContextDottydoc(val ctx: Context) extends AnyVal {
    def docbase: ContextDottydoc = ctx.docCtx.getOrElse {
      throw new IllegalStateException("DocBase must be set before running dottydoc phases")
    }.asInstanceOf[ContextDottydoc]
  }

  implicit class SymbolExtensions(val sym: Symbol) extends AnyVal {
    def sourcePosition(pos: Position)(implicit ctx: Context): SourcePosition =
      ctx.getSource(sym.sourceFile).atPos(pos)
  }
}
