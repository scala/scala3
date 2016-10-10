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
  with HideNonSensicalMessages {

  override def printSummary(implicit ctx: Context): Unit = delegate.printSummary()

  def doReport(cont: MessageContainer)(implicit ctx: Context): Unit = {
    val severity =
      cont match {
        case _: messages.Error   => xsbti.Severity.Error
        case _: messages.Warning => xsbti.Severity.Warn
        case _                   => xsbti.Severity.Info
      }
    val pos =
      if (cont.pos.exists) Some(cont.pos)
      else None

    val file =
      if (cont.pos.source.file.exists) Option(cont.pos.source.file.file)
      else None

    val offset0 = pos.map(_.point)

    val position = new Position {
      def line: Maybe[Integer] = maybe(pos.map(_.line))
      def lineContent: String = pos.map(_.lineContent).getOrElse("")
      def offset: Maybe[Integer] = maybeInt(offset0)
      def pointer: Maybe[Integer] = offset
      def pointerSpace: Maybe[String] = maybe(offset0.map(" " * _))
      def sourceFile: Maybe[java.io.File] = maybe(file)
      def sourcePath: Maybe[String] = maybe(file.map(_.getPath))
    }

    delegate.log(position, cont.message, severity)
  }

  private[this] def maybe[T](opt: Option[T]): Maybe[T] = opt match {
    case None => Maybe.nothing[T]
    case Some(s) => Maybe.just[T](s)
  }
  import java.lang.{ Integer => I }
  private[this] def maybeInt(opt: Option[Int]): Maybe[I] = opt match {
    case None => Maybe.nothing[I]
    case Some(s) => Maybe.just[I](s)
  }
}
