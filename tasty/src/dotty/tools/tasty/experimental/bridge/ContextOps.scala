package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag
import java.nio.file.Path

trait ContextOps extends Core with

  given ContextOps: (ctx: Context) extended with
    def log(msg: => String, sourcePos: SourcePosition): Unit = internal.Context_log(ctx, msg, sourcePos)
    def source: SourceFile = internal.Context_source(ctx)
    def docCtx: Option[ContextDocstrings] = internal.Context_docCtx(ctx)
    def withOwner(owner: Symbol): Context = internal.Context_withOwner(ctx, owner)
    def withSource(source: SourceFile): Context = internal.Context_withSource(ctx, source)

  given ContextDocstringsOps: (ctx: ContextDocstrings) extended with
    def docstring(sym: Symbol): Option[Comment] = internal.ContextDocstrings_docstring(ctx, sym)
