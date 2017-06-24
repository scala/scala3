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
import xsbti.{Maybe, Position}

final class DelegatingReporter(delegate: xsbti.Reporter) extends Reporter
  with UniqueMessagePositions
  with HideNonSensicalMessages
  with MessageRendering {
  import MessageContainer._

  override def printSummary(implicit ctx: Context): Unit = delegate.printSummary()

  def doReport(cont: MessageContainer)(implicit ctx: Context): Unit = {
    val severity =
      cont match {
        case _: messages.Error   => xsbti.Severity.Error
        case _: messages.Warning => xsbti.Severity.Warn
        case _                   => xsbti.Severity.Info
      }

    val position =
      if (false && cont.pos.exists) { // Disabled because it duplicates the information printed by Dotty
        val pos = cont.pos
        val src = pos.source
        new Position {
          val sourceFile: Maybe[java.io.File] = maybe(Option(src.file.file))
          val sourcePath: Maybe[String] = maybe(Option(src.file.path))
          val line: Maybe[Integer] = Maybe.just(pos.line)
          val lineContent: String = pos.lineContent.stripLineEnd
          val offset: Maybe[Integer] = Maybe.just(pos.point)
          val pointer: Maybe[Integer] = Maybe.just(pos.point - src.startOfLine(pos.point))
          val pointerSpace: Maybe[String] = Maybe.just(
            ((lineContent: Seq[Char]).take(pointer.get).map { case '\t' => '\t'; case x => ' ' }).mkString
          )
        }
      } else
        noPosition

    val sb = new StringBuilder()
    sb.append(messageAndPos(cont.contained(), cont.pos, diagnosticLevel(cont)))
    if (ctx.shouldExplain(cont) && cont.contained().explanation.nonEmpty) {
      sb.append(explanation(cont.contained()))
    }

    delegate.log(position, sb.toString(), severity)
  }

  private[this] def maybe[T](opt: Option[T]): Maybe[T] = opt match {
    case None => Maybe.nothing[T]
    case Some(s) => Maybe.just[T](s)
  }

  private[this] val noPosition = new Position {
    val line: Maybe[Integer] = Maybe.nothing[Integer]
    val lineContent: String = ""
    val offset: Maybe[Integer] = Maybe.nothing[Integer]
    val pointer: Maybe[Integer] = Maybe.nothing[Integer]
    val pointerSpace: Maybe[String] = Maybe.nothing[String]
    val sourceFile: Maybe[java.io.File] = Maybe.nothing[java.io.File]
    val sourcePath: Maybe[String] = Maybe.nothing[String]
  }
}
