/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package dotty.tools.dotc.util

import scala.language.unsafeNulls

import collection.mutable, mutable.ListBuffer
import scala.util.chaining.given
import java.lang.System.lineSeparator

object StackTraceOps:

  extension (t: Throwable)

    /** Format a stack trace, taking the prefix span satisfying a predicate.
     *
     *  The format is similar to the typical case described in the Javadoc
     *  for [[java.lang.Throwable#printStackTrace()*]].
     *  If a stack trace is truncated, it will be followed by a line of the form
     *  `... 3 elided`, by analogy to the lines `... 3 more` which indicate
     *  shared stack trace segments.
     *  @param e the exception
     *  @param p the predicate to select the prefix
     */
    def formatStackTracePrefix(p: StackTraceElement => Boolean): String =

      type TraceRelation = String
      val Self       = new TraceRelation("")
      val CausedBy   = new TraceRelation("Caused by: ")
      val Suppressed = new TraceRelation("Suppressed: ")

      def header(e: Throwable): String =
        def because = e.getCause   match { case null => null    ; case c => header(c) }
        def msg     = e.getMessage match { case null => because ; case s => s         }
        def txt     = msg          match { case null => ""      ; case s => s": $s"   }
        s"${e.getClass.getName}$txt"

      val seen = mutable.Set.empty[Throwable]
      def unseen(e: Throwable): Boolean = (e != null && !seen(e)).tap(if _ then seen += e)

      val lines = ListBuffer.empty[String]

      // format the stack trace, skipping the shared trace
      def print(e: Throwable, r: TraceRelation, share: Array[StackTraceElement], indents: Int): Unit = if unseen(e) then
        val trace  = e.getStackTrace
        val frames = if share.isEmpty then trace else
          val spare   = share.reverseIterator
          val trimmed = trace.reverse.dropWhile(spare.hasNext && spare.next() == _)
          trimmed.reverse
        val prefix = frames.takeWhile(p)
        val margin = "  " * indents
        lines += s"${margin}${r}${header(e)}"
        prefix.foreach(frame => lines += s"$margin  at $frame")

        val traceFramesLenDiff  = trace.length - frames.length
        val framesPrefixLenDiff = frames.length - prefix.length
        if traceFramesLenDiff > 0 then
          if framesPrefixLenDiff > 0 then lines += s"$margin  ... $framesPrefixLenDiff elided and $traceFramesLenDiff more"
          else lines += s"$margin  ... $traceFramesLenDiff more"
        else if framesPrefixLenDiff > 0 then lines += s"$margin  ... $framesPrefixLenDiff elided"

        print(e.getCause, CausedBy, trace, indents)
        e.getSuppressed.foreach(print(_, Suppressed, frames, indents + 1))
      end print

      print(t, Self, share = Array.empty, indents = 0)
      lines.mkString(lineSeparator)
    end formatStackTracePrefix
