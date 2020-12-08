package dotty.tools
package languageserver
package decompiler

import java.net.URI
import java.nio.file._
import java.util.concurrent.CompletableFuture

import dotty.tools.tasty.UnpickleException
import dotty.tools.io.{PlainFile, Path}

import dotc.fromtasty.TastyFileUtil

import org.eclipse.lsp4j.jsonrpc.services._


@JsonSegment("tasty")
trait TastyDecompilerService {
  thisServer: DottyLanguageServer =>

  @JsonRequest
  def decompile(params: TastyDecompileParams): CompletableFuture[TastyDecompileResult] =
    computeAsync(synchronize = false, fun = { cancelChecker =>
      val uri = new URI(params.textDocument.getUri)
      try {
        val jpath = Paths.get(uri)
        val tastyFile = new PlainFile(Path(jpath))
        TastyFileUtil.getClassPath(tastyFile) match {
          case Some(classPath) =>
            val driver = thisServer.decompilerDriverFor(uri, classPath)

            val (tree, source) = driver.run(tastyFile)

            TastyDecompileResult(tree, source)
          case _ =>
            TastyDecompileResult(error = TastyDecompileResult.ErrorClassNotFound)
        }
      } catch {
        case _: UnpickleException =>
          TastyDecompileResult(error = TastyDecompileResult.ErrorTastyVersion)
        case t: Throwable =>
          t.printStackTrace()
          TastyDecompileResult(error = TastyDecompileResult.ErrorOther)
      }
    })
}
