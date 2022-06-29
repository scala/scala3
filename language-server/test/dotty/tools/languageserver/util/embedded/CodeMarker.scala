package dotty.tools.languageserver.util.embedded

import dotty.tools.languageserver.util.server.TestFile
import dotty.tools.languageserver.util.{CodeRange, PositionContext}

import scala.jdk.CollectionConverters._

import org.eclipse.lsp4j._

import PositionContext.PosCtx

/** Used to mark positions in the code */
class CodeMarker(val name: String) extends Embedded {

  /** A range of positions between this marker and `other`. */
  def to(other: CodeMarker): CodeRange = CodeRange(this, other)

  /** The file containing this marker. */
  def file: PosCtx[TestFile] = posCtx.positionOf(this)._1

  /** The uri of the file containing this marker. */
  def uri: PosCtx[String] = file.uri

  /** The line containing this marker. */
  def line: PosCtx[Int] = posCtx.positionOf(this)._2

  /** The columng number of this marker. */
  def character: PosCtx[Int] = posCtx.positionOf(this)._3

  /** Converts this marker to a position. */
  def toPosition: PosCtx[Position] = new Position(line, character)

  def toTextDocumentPositionParams: PosCtx[TextDocumentPositionParams] =
    new TextDocumentPositionParams(toTextDocumentIdentifier, toPosition)

  def toDocumentSymbolParams: PosCtx[DocumentSymbolParams] =
    new DocumentSymbolParams(toTextDocumentIdentifier)

  def toCompletionParams: PosCtx[CompletionParams] =
    new CompletionParams(toTextDocumentIdentifier, toPosition)

  def toPublishDiagnosticsParams(diagnostics: List[Diagnostic]): PosCtx[PublishDiagnosticsParams] =
    new PublishDiagnosticsParams(file.uri, diagnostics.asJava)

  def toRenameParams(newName: String): PosCtx[RenameParams] =
    new RenameParams(toTextDocumentIdentifier, toPosition, newName)

  def toTextDocumentIdentifier: PosCtx[TextDocumentIdentifier] =
    new TextDocumentIdentifier(file.uri)

  def toVersionedTextDocumentIdentifier: PosCtx[VersionedTextDocumentIdentifier] =
    new VersionedTextDocumentIdentifier(file.uri, 0)

  def toReferenceParams(withDecl: Boolean): PosCtx[ReferenceParams] = {
    val rp = new ReferenceParams(new ReferenceContext(withDecl))
    rp.setTextDocument(toTextDocumentIdentifier)
    rp.setPosition(toPosition)
    rp
  }

  def show: PosCtx[String] = s"($name,line=$line,char=$character)"
  override def toString: String = s"CodePosition($name)"

  private implicit def posCtx(implicit ctx: PositionContext): PositionContext = ctx
}
