package dotty.tools.scaladoc
package snippets

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import dotty.tools.dotc.config.Feature
import dotty.tools.dotc.util.{ NoSource, SourceFile, SourcePosition }
import dotty.tools.dotc.util.Spans.Span

case class WrappedSnippet(
  snippet: String,
  outerLineOffset: Int,
  outerColumnOffset: Int,
  innerLineOffset: Int,
  innerColumnOffset: Int,
  lineMappings: IndexedSeq[Option[WrappedSnippet.WrappedLineMapping]]
):
  def sourcePosition(diagPos: SourcePosition, sourceFile: SourceFile): Option[Position] =
    lineMappings
      .lift(diagPos.line)
      .flatten
      .flatMap(_.sourcePosition(diagPos, sourceFile, outerColumnOffset))

  def sourceSpanPosition(diagPos: SourcePosition, sourceFile: SourceFile): Option[Position] =
    lineMappings
      .lift(diagPos.line)
      .flatten
      .flatMap(_.sourceSpanPosition(diagPos, sourceFile, outerColumnOffset))

object WrappedSnippet:
  // `wrappedColumnOffset` accounts for indentation added by the synthetic wrapper.
  case class WrappedLineMapping(sourceLine: Int, relativeLine: Int, wrappedColumnOffset: Int):
    def sourcePosition(
      diagPos: SourcePosition,
      sourceFile: SourceFile,
      outerColumnOffset: Int
    ): Option[Position] =
      mapPosition(diagPos, sourceFile, outerColumnOffset, diagPos.column, diagPos.column)

    def sourceSpanPosition(
      diagPos: SourcePosition,
      sourceFile: SourceFile,
      outerColumnOffset: Int
    ): Option[Position] =
      val endColumn =
        if diagPos.startLine == diagPos.endLine then diagPos.endColumn
        else diagPos.startColumn
      mapPosition(diagPos, sourceFile, outerColumnOffset, diagPos.startColumn, endColumn)

    private def mapPosition(
      diagPos: SourcePosition,
      sourceFile: SourceFile,
      outerColumnOffset: Int,
      startBase: Int,
      endBase: Int
    ): Option[Position] =
      val lineOffset = sourceFile match
        case NoSource => Some(0)
        case sf: SourceFile => sf.lineToOffsetOpt(sourceLine)
      lineOffset.map: offset =>
        val startColumn = (startBase + outerColumnOffset - wrappedColumnOffset).max(0)
        val endColumn = (endBase + outerColumnOffset - wrappedColumnOffset).max(startColumn)
        val span = Span(offset + startColumn, offset + endColumn)
        Position(SourcePosition(sourceFile, span), relativeLine)

  val indent: Int = 2

  /** Matches import lines for global language features that must be at the toplevel. */
  private val globalLanguageImport =
    val names = Feature.globalLanguageImports.map(_.toString.stripPrefix("experimental."))
    raw"import\s+language\s*\.\s*experimental\s*\.\s*(${names.mkString("|")})\b".r.unanchored

  private def isGlobalLanguageImport(line: String): Boolean =
    globalLanguageImport.matches(line.trim)

  def apply(
    str: String,
    packageName: Option[String],
    outerLineOffset: Int,
    outerColumnOffset: Int,
  ): WrappedSnippet =
    val sourceLines = splitLines(str).indices.map(idx => Some(outerLineOffset + idx))
    apply(str, packageName, outerLineOffset, outerColumnOffset, sourceLines)

  def apply(
    str: String,
    packageName: Option[String],
    outerLineOffset: Int,
    outerColumnOffset: Int,
    sourceLines: Seq[Option[Int]],
  ): WrappedSnippet =
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)

    val lines = splitLines(str)
    require(
      lines.length == sourceLines.length,
      s"Source lines length ${sourceLines.length} did not match snippet line count ${lines.length}"
    )
    val indexedLines = lines.zip(sourceLines).zipWithIndex.map:
      case ((line, sourceLine), relativeLine) => (line, sourceLine, relativeLine)
    val (globalImports, rest) = indexedLines.partition:
      case (line, _, _) => isGlobalLanguageImport(line)
    val lineMappings = IndexedSeq.newBuilder[Option[WrappedLineMapping]]

    def printlnWithMapping(str: String, mapping: Option[WrappedLineMapping]) =
      ps.println(str)
      lineMappings += mapping

    ps.startHide()
    lineMappings += None
    ps.println(s"package ${packageName.getOrElse("snippets")}")
    lineMappings += None
    // Global language imports must stay at the top level instead of inside `Snippet`.
    globalImports.foreach:
      case (line, sourceLine, relativeLine) =>
        printlnWithMapping(
          line,
          sourceLine.map(WrappedLineMapping(_, relativeLine, 0))
        )

    ps.println("object Snippet {")
    lineMappings += None
    ps.endHide()
    lineMappings += None

    rest.foreach:
      case (line, sourceLine, relativeLine) =>
        printlnWithMapping(
          (" " * indent) + line,
          sourceLine.map(WrappedLineMapping(_, relativeLine, indent))
        )

    ps.startHide()
    lineMappings += None
    ps.println("}")
    lineMappings += None
    ps.endHide()
    lineMappings += None

    WrappedSnippet(
      baos.toString,
      outerLineOffset,
      outerColumnOffset,
      2 + globalImports.length + 2 /*Hide tokens*/,
      indent,
      lineMappings.result()
    )

  def splitLines(str: String): IndexedSeq[String] =
    if str.isEmpty then IndexedSeq.empty
    else str.split('\n').toIndexedSeq

  extension (ps: PrintStream)
    private def startHide() = ps.println(raw"//{")
    private def endHide() = ps.println(raw"//}")
