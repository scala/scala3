package dotty.tools
package languageserver
package decompiler

import java.net.URI
import java.nio.file._
import java.util.concurrent.CompletableFuture

import dotc.core.tasty.UnpickleException
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
        TastyFileUtil.getClassName(Paths.get(uri)) match {
          case Some((classPath, className)) =>
            val driver = thisServer.decompilerDriverFor(uri, classPath)

            val (tree, source) = driver.run(className)

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
