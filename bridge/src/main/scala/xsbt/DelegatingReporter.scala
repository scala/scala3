/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import dotty.tools._
import dotc._
import reporting._
import core.Contexts._

import xsbti.Maybe

object DelegatingReporter {
  def apply(delegate: xsbti.Reporter) = new DelegatingReporter(delegate)
}

final class DelegatingReporter(delegate: xsbti.Reporter) extends Reporter
  with UniqueMessagePositions
  with HideNonSensicalMessages {

  def doReport(d: Diagnostic)(implicit ctx: Context): Unit = {
    val severity = 
      d match {
        case _: Reporter.Error   => xsbti.Severity.Error
        case _: Reporter.Warning => xsbti.Severity.Warn
        case _                   => xsbti.Severity.Info
      }
    val pos  = 
      if(d.pos.exists) Some(d.pos)
      else None

    val file = 
      if(d.pos.source.file.exists) {
        val r = d.pos.source.file.file 
        if(r == null) None
        else Some(r)
      }
      else None

    val offset0 = pos.map(_.point)

    val position = new xsbti.Position {
      def line: Maybe[Integer] = maybe(pos.map(_.line))
      def lineContent(): String = pos.map(_.lineContent).getOrElse("")
      def offset(): xsbti.Maybe[Integer] = maybeInt(offset0)
      def pointer(): xsbti.Maybe[Integer] = offset()
      def pointerSpace(): xsbti.Maybe[String] = maybe(offset0.map(" " * _))
      def sourceFile(): xsbti.Maybe[java.io.File] = maybe(file)
      def sourcePath(): xsbti.Maybe[String] = maybe(file.map(_.getPath))
    }

    delegate.log(position, d.message, severity)
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