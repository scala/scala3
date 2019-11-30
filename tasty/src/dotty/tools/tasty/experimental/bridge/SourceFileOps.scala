package dotty.tools.tasty.experimental.bridge

import reflect.ClassTag

trait SourceFileOps extends Core with

  object SourceFile with
    val noSource: SourceFile = internal.SourceFile_noSource

  given SourceFileOps: (source: SourceFile) with
    def path: String = internal.SourceFile_path(source)
    def exists: Boolean = internal.SourceFile_exists(source)
