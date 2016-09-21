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
    val pos  = d.pos

    val file = 
      if(pos.source.file.exists) Some(pos.source.file.file)
      else None

    val position = new xsbti.Position {
      def line: Maybe[Integer] = Maybe.just(pos.line)
      def lineContent(): String = pos.lineContent
      def offset(): xsbti.Maybe[Integer] = Maybe.just(pos.point)
      def pointer(): xsbti.Maybe[Integer] = offset()
      def pointerSpace(): xsbti.Maybe[String] = Maybe.just(" " * pos.point)
      def sourceFile(): xsbti.Maybe[java.io.File] = maybe(file)
      def sourcePath(): xsbti.Maybe[String] = {
        println(file)
        maybe(file.map(_.getPath))
      }
    }

    delegate.log(position, d.message, severity)
  }

  private[this] def maybe[T](opt: Option[T]): Maybe[T] = opt match { 
    case None => Maybe.nothing[T]
    case Some(s) => Maybe.just[T](s)
  }
}