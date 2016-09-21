package dotty.tools
package dottydoc
package util

import dotc.core.Contexts.Context
import dotc.core.Comments._
import model.Package
import core.ContextDottydoc

object syntax {
  implicit class ContextWithContextDottydoc(val ctx: Context) extends AnyVal {
    def docbase: ContextDottydoc = ctx.docCtx.getOrElse {
      throw new IllegalStateException("DocBase must be set before running dottydoc phases")
    }.asInstanceOf[ContextDottydoc]
  }
}
