package dotty.tools.pc

import java.io.ByteArrayOutputStream
import java.net.URI
import java.nio.file.Path
import java.nio.file.Paths

import scala.meta.internal.mtags.MD5
import scala.util.Properties

import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.dotc.semanticdb
import dotty.tools.dotc.util.SourceFile

class SemanticdbTextDocumentProvider(
    driver: InteractiveDriver,
    workspace: Option[Path]
) extends WorksheetSemanticdbProvider:

  def textDocument(
      uri: URI,
      sourceCode: String
  ): Array[Byte] =
    val filePath = Paths.get(uri).nn
    val validCode = removeMagicImports(sourceCode, filePath)
    driver.run(
      uri,
      SourceFile.virtual(filePath.toString(), validCode)
    )
    val tree = driver.currentCtx.run.nn.units.head.tpdTree
    val path = workspace
      .flatMap { workspacePath =>
        scala.util.Try(workspacePath.relativize(filePath)).toOption
      }
      .map { relativeUri =>
        if Properties.isWin then relativeUri.toString().replace("\\", "/")
        else relativeUri.toString()
      }
      .getOrElse(filePath.toString())

    semanticdb.textDocumentBytes(tree, path.nn, sourceCode)(using driver.currentCtx)

end SemanticdbTextDocumentProvider
