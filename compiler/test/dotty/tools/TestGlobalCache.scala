package dotty.tools

import dotty.tools.dotc.GlobalCache
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.io.{AbstractFile, FileExtension}
import java.io.File

object TestGlobalCache extends GlobalCache.ConcurrentGlobalCache(
  file => {
    if file.ext == FileExtension.Class || file.ext == FileExtension.Tasty then
      !file.canonicalPath.startsWith("out")
    else if file.ext == FileExtension.Scala then
      file.canonicalPath.startsWith("library/src")
    else
      false
  }
)
