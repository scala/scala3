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
      if (cont.pos.exists)
        new Position {
          val line: Maybe[Integer] = Maybe.just(cont.pos.line)
          val lineContent: String = cont.pos.lineContent
          val offset: Maybe[Integer] = Maybe.just(cont.pos.point)
          val pointer: Maybe[Integer] = Maybe.just(cont.pos.point)
          val pointerSpace: Maybe[String] = Maybe.just(" " * cont.pos.point)
          val sourceFile: Maybe[java.io.File] = maybe(Option(cont.pos.source.file.file))
          val sourcePath: Maybe[String] = maybe(Option(cont.pos.source.file.file).map(_.getPath))
        }
      else
        noPosition

    val sb = new StringBuilder()
    sb.append(messageAndPos(cont.contained, cont.pos, diagnosticLevel(cont)))
    if (ctx.shouldExplain(cont) && cont.contained.explanation.nonEmpty) {
      sb.append(explanation(cont.contained))
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
