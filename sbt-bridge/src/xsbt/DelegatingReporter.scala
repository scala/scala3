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

    val position = new Position {
      def line: Maybe[Integer] = Maybe.nothing()
      def lineContent: String = ""
      def offset: Maybe[Integer] = Maybe.nothing()
      def pointer: Maybe[Integer] = Maybe.nothing()
      def pointerSpace: Maybe[String] = Maybe.nothing()
      def sourceFile: Maybe[java.io.File] = Maybe.nothing()
      def sourcePath: Maybe[String] = Maybe.nothing()
    }

    val sb = new StringBuilder()
    sb.append(messageAndPos(cont.contained, cont.pos, diagnosticLevel(cont)))
    if (ctx.shouldExplain(cont) && cont.contained.explanation.nonEmpty) {
      sb.append(explanation(cont.contained))
    }

    delegate.log(position, sb.toString(), severity)
  }
}
