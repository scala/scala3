package scala.quoted.compiletime.internal

import dotty.tools.dotc
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.quoted.MacroExpansion
import scala.quoted.compiletime as pub

type Position = PositionImpl
final case class PositionImpl(position: dotc.util.SourcePosition) extends pub.Position {

  override lazy val start: Int = position.start

  override lazy val end: Int = position.end

  override lazy val sourceFile: SourceFile = SourceFileImpl(position.source)

  override lazy val startLine: Int = position.startLine

  override lazy val endLine: Int = position.endLine

  override lazy val startColumn: Int = position.startColumn

  override lazy val endColumn: Int = position.endColumn

  override lazy val sourceCode: Option[String] =
    val contents = position.source.content()
    if contents.length < position.end then None
    else Some(new String(contents, position.start, position.end - position.start))

  override def toString: String = position.toString
  override lazy val hashCode: Int = position.hashCode
  override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: PositionImpl => this.position == that.position
    case _                  => false

}
object PositionImpl {

  final class Module(using val ctx: Context) extends pub.Position.Module {

    override def ofMacroExpansion: Position =
      PositionImpl(MacroExpansion.position.getOrElse(dotc.util.SourcePosition(ctx.source, dotc.util.Spans.NoSpan)))

    override def apply(sourceFile: pub.SourceFile, start: Int, end: Int): Position =
      make(sourceFile, start, end)

    override def make(sourceFile: pub.SourceFile, start: Int, end: Int): Position =
      PositionImpl(dotc.util.SourcePosition(sourceFile.cast.sourceFile, dotc.util.Spans.Span(start, end)))

  }

}
