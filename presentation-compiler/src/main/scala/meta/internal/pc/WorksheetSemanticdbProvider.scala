package scala.meta.internal.pc

import java.nio.file.Path

import scala.meta.internal.mtags.MtagsEnrichments.*

trait WorksheetSemanticdbProvider:

  private val magicImportsRegex =
    """import\s+(\$ivy|\$repo|\$dep|\$scalac)\..*""".r

  def removeMagicImports(code: String, filePath: Path): String =
    val absoluteFilePath = filePath.toAbsolutePath()
    if absoluteFilePath.toString.isWorksheet then
      code.linesIterator
        .map {
          case magicImportsRegex(_) => ""
          case other => other
        }
        .mkString("\n")
    else code
end WorksheetSemanticdbProvider
