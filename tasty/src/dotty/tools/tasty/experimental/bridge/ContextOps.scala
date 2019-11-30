package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag
import java.nio.file.Path

trait ContextOps extends Core with

  given ContextOps: (ctx: Context) with
    def log(msg: => String, sourcePos: SourcePosition): Unit = internal.Context_log(ctx, msg, sourcePos)
    def source: Path = internal.Context_source(ctx)
