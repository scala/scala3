/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import dotty.tools._
import dotc._
import reporting._
import reporting.diagnostic.MessageContainer
import reporting.diagnostic.messages
import core.Contexts._
import xsbti.{Position, Severity}
import java.util.Optional

final class DelegatingReporter(delegate: xsbti.Reporter) extends Reporter
  with UniqueMessagePositions
  with HideNonSensicalMessages
  with MessageRendering {
  import MessageContainer._

  override def printSummary(implicit ctx: Context): Unit = delegate.printSummary()

  def doReport(cont: MessageContainer)(implicit ctx: Context): Unit = {
    val severity =
      cont match {
        case _: messages.Error   => Severity.Error
        case _: messages.Warning => Severity.Warn
        case _                   => Severity.Info
      }

    val position =
      if (cont.pos.exists) {
        val pos = cont.pos
        val src = pos.source
        new Position {
          val sourceFile: Optional[java.io.File] = maybe(Option(src.file.file))
          val sourcePath: Optional[String] = maybe(Option(src.file.path))
          val line: Optional[Integer] = Optional.of(pos.line)
          val lineContent: String = pos.lineContent.stripLineEnd
          val offset: Optional[Integer] = Optional.of(pos.point)
          val pointer: Optional[Integer] = Optional.of(pos.point - src.startOfLine(pos.point))
          val pointerSpace: Optional[String] = Optional.of(
            ((lineContent: Seq[Char]).take(pointer.get).map { case '\t' => '\t'; case x => ' ' }).mkString
          )
        }
      } else
        noPosition

    val message = cont.contained()
    val rendered = new StringBuilder()
    rendered.append(messageAndPos(message, cont.pos, diagnosticLevel(cont)))
    if (ctx.shouldExplain(cont) && message.explanation.nonEmpty) {
      rendered.append(explanation(message))
    }

    delegate.log(Problem(position, message.msg, severity, rendered.toString))
  }

  private[this] def maybe[T](opt: Option[T]): Optional[T] = opt match {
    case None => Optional.empty[T]
    case Some(s) => Optional.of[T](s)
  }

  private[this] val noPosition = new Position {
    val line: Optional[Integer] = Optional.empty[Integer]
    val lineContent: String = ""
    val offset: Optional[Integer] = Optional.empty[Integer]
    val pointer: Optional[Integer] = Optional.empty[Integer]
    val pointerSpace: Optional[String] = Optional.empty[String]
    val sourceFile: Optional[java.io.File] = Optional.empty[java.io.File]
    val sourcePath: Optional[String] = Optional.empty[String]
  }
}
