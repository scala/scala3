package dotty.tools
package dotc
package core

import Types.*, Contexts.*, Symbols.*, Decorators.*
import util.Property
import Names.Name

/** A utility module to produce match type reduction traces in error messages.
 */
object MatchTypeTrace:

  private enum TraceEntry:
    case TryReduce(scrut: Type)
    case Stuck(scrut: Type, stuckCase: MatchTypeCaseSpec, otherCases: List[MatchTypeCaseSpec])
    case NoInstance(scrut: Type, stuckCase: MatchTypeCaseSpec, fails: List[(Name, TypeBounds)])
    case EmptyScrutinee(scrut: Type)
  import TraceEntry.*

  private class MatchTrace:
    var entries: List[TraceEntry] = Nil

  private val MatchTrace = new Property.Key[MatchTrace]

  /** Execute `op` and if it involves a failed match type reduction
   *  return the trace of that reduction. Otherwise return the empty string.
   */
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

  /** Are we running an operation that records a match type trace? */
  def isRecording(using Context): Boolean =
    ctx.property(MatchTrace).isDefined

  private def matchTypeFail(entry: TraceEntry)(using Context) =
    ctx.property(MatchTrace) match
      case Some(trace) =>
        trace.entries match
          case (e: TryReduce) :: es => trace.entries = entry :: trace.entries
          case _ =>
      case _ =>

  /** Record a failure that scrutinee `scrut` does not match `stuckCase` but is
   *  not disjoint from it either, which means that the remaining cases `otherCases`
   *  cannot be visited. Only the first failure is recorded.
   */
  def stuck(scrut: Type, stuckCase: MatchTypeCaseSpec, otherCases: List[MatchTypeCaseSpec])(using Context) =
    matchTypeFail(Stuck(scrut, stuckCase, otherCases))

  def noInstance(scrut: Type, stuckCase: MatchTypeCaseSpec, fails: List[(Name, TypeBounds)])(using Context) =
    matchTypeFail(NoInstance(scrut, stuckCase, fails))

  /** Record a failure that scrutinee `scrut` is provably empty.
   *  Only the first failure is recorded.
   */
  def emptyScrutinee(scrut: Type)(using Context) =
    matchTypeFail(EmptyScrutinee(scrut))

  /** Record in the trace that we are trying to reduce `scrut` when performing `op`
   *  If `op` succeeds the entry is removed after exit. If `op` fails, it stays.
   */
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

  def caseText(spec: MatchTypeCaseSpec)(using Context): String =
    caseText(spec.origMatchCase)

  def caseText(tp: Type)(using Context): String = tp match
    case tp: HKTypeLambda => caseText(tp.resultType)
    case defn.MatchCase(any, body) if any eq defn.AnyType => i"case _ => $body"
    case defn.MatchCase(pat, body) => i"case $pat => $body"
    case _ => i"case $tp"

  private def casesText(cases: List[MatchTypeCaseSpec])(using Context) =
    i"${cases.map(caseText)}%\n    %"

  private def explainEntry(entry: TraceEntry)(using Context): String = entry match
    case TryReduce(scrut: Type) =>
      i"  trying to reduce  $scrut"
    case EmptyScrutinee(scrut) =>
      i"""  failed since selector $scrut
         |  is uninhabited (there are no values of that type)."""
    case Stuck(scrut, stuckCase, otherCases) =>
      val msg =
        i"""  failed since selector $scrut
           |  does not match  ${caseText(stuckCase)}
           |  and cannot be shown to be disjoint from it either."""
      if otherCases.length == 0 then msg
      else
        val s = if otherCases.length == 1 then "" else "s"
        i"""$msg
           |  Therefore, reduction cannot advance to the remaining case$s
           |
           |    ${casesText(otherCases)}"""
    case NoInstance(scrut, stuckCase, fails) =>
      def params = if fails.length == 1 then "parameter" else "parameters"
      i"""  failed since selector $scrut
         |  does not uniquely determine $params ${fails.map(_._1)}%, % in
         |    ${caseText(stuckCase)}
         |  The computed bounds for the $params are:
         |    ${fails.map((name, bounds) => i"$name$bounds")}%\n    %"""

  /** The failure message when the scrutinee `scrut` does not match any case in `cases`. */
  def noMatchesText(scrut: Type, cases: List[MatchTypeCaseSpec])(using Context): String =
    i"""failed since selector $scrut
       |matches none of the cases
       |
       |    ${casesText(cases)}"""

end MatchTypeTrace
