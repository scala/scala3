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

package dotty.tools.repl

import scala.language.unsafeNulls
import collection.mutable, mutable.ListBuffer
import dotty.tools.dotc.util.chaining.*
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
     *  @param p the predicate to select the prefix
     */
    def formatStackTracePrefix(p: StackTraceElement => Boolean): fansi.Str =

      type TraceRelation = String
      val Self       = ""
      val CausedBy   = "Caused by: "
      val Suppressed = "Suppressed: "
      def renderDotDelimited(s: String ) = {
        fansi.Str.join(s.split('.').map(fansi.Color.LightRed(_)), ".")
      }

      def header(e: Throwable): fansi.Str =
        def because = e.getCause   match { case null => null    ; case c => header(c) }
        def msg     = e.getMessage match { case null => because ; case s => s         }
        def txt  = msg match {
          case null => ""
          case s => s": $s"
        }
        renderDotDelimited(e.getClass.getName) ++ txt

      val seen = mutable.Set.empty[Throwable]
      def unseen(e: Throwable): Boolean = (e != null && !seen(e)).tap(if _ then seen += e)

      val lines = ListBuffer.empty[fansi.Str]
      // format the stack trace, skipping the shared trace
      def print(e: Throwable, r: TraceRelation, share: Array[StackTraceElement], indents: Int): Unit = if unseen(e) then
        val trace  = e.getStackTrace
        val frames = if share.isEmpty then trace else
          val spare   = share.reverseIterator
          val trimmed = trace.reverse.dropWhile(spare.hasNext && spare.next() == _)
          trimmed.reverse
        val prefix = frames.takeWhile(p)
        val margin = "  " * indents
        lines += fansi.Str(margin) ++ fansi.Color.Red(r) ++ header(e)
        prefix.foreach(frame =>
          lines +=
            fansi.Str(s"$margin  ") ++ fansi.Color.Red("at") ++ " " ++
              renderDotDelimited(frame.getClassName) ++ "." ++
              fansi.Color.LightRed(frame.getMethodName) ++ "(" ++
              fansi.Color.LightRed(frame.getFileName) ++ ":" ++
              fansi.Color.LightRed(frame.getLineNumber.toString) ++ ")"
        )

        val traceFramesLenDiff  = trace.length - frames.length
        val framesPrefixLenDiff = frames.length - prefix.length
        if traceFramesLenDiff > 0 then
          if framesPrefixLenDiff > 0
          then lines += fansi.Color.Red(s"$margin  ... $framesPrefixLenDiff elided and $traceFramesLenDiff more")
          else lines += fansi.Color.Red(s"$margin  ... $traceFramesLenDiff more")
        else if framesPrefixLenDiff > 0
        then lines += fansi.Color.Red(s"$margin  ... $framesPrefixLenDiff elided")

        print(e.getCause, CausedBy, trace, indents)
        e.getSuppressed.foreach(print(_, Suppressed, frames, indents + 1))
      end print

      print(t, Self, share = Array.empty, indents = 0)
      fansi.Str.join(lines, lineSeparator)
    end formatStackTracePrefix
