package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._, Decorators._
import util.Property

object MatchTypeTrace:
  private enum TraceEntry:
    case TryReduce(scrut: Type)
    case NoMatches(scrut: Type, cases: List[Type])
    case Stuck(scrut: Type, stuckCase: Type, otherCases: List[Type])
  import TraceEntry._

  private class MatchTrace:
    var entries: List[TraceEntry] = Nil

  private val MatchTrace = new Property.Key[MatchTrace]

  def record(op: Context ?=> Any)(using Context): String =
    val trace = new MatchTrace
    inContext(ctx.fresh.setProperty(MatchTrace, trace)) {
      op
      if trace.entries.isEmpty then ""
      else
        i"""
           |
           |Note: a match type could not be fully reduced:
           |
           |${trace.entries.reverse.map(explainEntry)}%\n%"""
    }

  def isRecording(using Context): Boolean =
    ctx.property(MatchTrace).isDefined

  private def matchTypeFail(entry: TraceEntry)(using Context) =
    ctx.property(MatchTrace) match
      case Some(trace) =>
        trace.entries match
          case (e: TryReduce) :: es => trace.entries = entry :: trace.entries
          case _ =>
      case _ =>

  def noMatches(scrut: Type, cases: List[Type])(using Context) =
    matchTypeFail(NoMatches(scrut, cases))

  def stuck(scrut: Type, stuckCase: Type, otherCases: List[Type])(using Context) =
    matchTypeFail(Stuck(scrut, stuckCase, otherCases))

  def recurseWith(scrut: Type)(op: => Type)(using Context): Type =
    ctx.property(MatchTrace) match
      case Some(trace) =>
        val prev = trace.entries
        trace.entries = TryReduce(scrut) :: prev
        val res = op
        if res.exists then trace.entries = prev
        res
      case _ =>
        op

  private def caseText(tp: Type)(using Context): String = tp match
    case tp: HKTypeLambda => caseText(tp.resultType)
    case defn.MatchCase(pat, body) => i"case $pat => $body"
    case _ => i"case $tp"

  private def casesText(cases: List[Type])(using Context) =
    i"${cases.map(caseText)}%\n    %"

  private def explainEntry(entry: TraceEntry)(using Context): String = entry match
    case TryReduce(scrut: Type) =>
      i"  trying to reduce  $scrut"
    case NoMatches(scrut, cases) =>
      i"""  failed since selector  $scrut
         |  matches none of the cases
         |
         |    ${casesText(cases)}"""
    case Stuck(scrut, stuckCase, otherCases) =>
      i"""  failed since selector  $scrut
         |  does not match  ${caseText(stuckCase)}
         |  and cannot be shown to be disjoint from it either.
         |  Therefore, reduction cannot advance to the remaining case${if otherCases.length == 1 then "" else "s"}
         |
         |    ${casesText(otherCases)}"""

end MatchTypeTrace

