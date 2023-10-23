package dotty.tools.dotc.sbt

import dotty.tools.DottyTest
import dotty.tools.dotc.core.Contexts.FreshContext
import dotty.tools.dotc.sbt.ProgressCallbackTest.*

import org.junit.Assert.*
import org.junit.Test

import dotty.tools.toOption
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.Compiler
import dotty.tools.dotc.Run
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.io.VirtualDirectory
import dotty.tools.dotc.NoCompilationUnit

final class ProgressCallbackTest extends DottyTest:

  @Test
  def testCallback: Unit =
    val source1 = """class Foo"""
    val source2 = """class Bar"""

    inspectProgress(List(source1, source2), terminalPhase = None): progressCallback =>
      // (1) assert that the way we compute next phase in `Run.doAdvancePhase` is correct
      assertNextPhaseIsNext()

      // (1) given correct computation, check that the recorded progression is monotonic
      assertMonotonicProgression(progressCallback)

      // (1) given monotonic progression, check that the recorded progression has full coverage
      assertFullCoverage(progressCallback)

      // (2) next check that for each unit, we record the expected phases that it should progress through
      assertExpectedPhases(progressCallback)

      // (2) therefore we can now cross-reference the recorded progression with the recorded phases per unit
      assertTotalUnits(progressCallback)

      // (3) finally, check that the callback was not cancelled
      assertFalse(progressCallback.isCancelled)
  end testCallback

  // TODO: test lateCompile, test cancellation

  /** Assert that the computed `next` phase matches the real next phase  */
  def assertNextPhaseIsNext()(using Context): Unit =
    val allPhases = ctx.base.allPhases
    for case Array(p1, p2) <- allPhases.sliding(2) do
      val p1Next = Run.SubPhases(p1).next.get.phase // used to compute the next phase in `Run.doAdvancePhase`
      assertEquals(p1Next, p2)

  /** Assert that the recorded progression of phases are all in the real progression, and that order is preserved */
  def assertMonotonicProgression(progressCallback: TestProgressCallback)(using Context): Unit =
    val allPhasePlan = ctx.base.allPhases.flatMap(asSubphases) ++ syntheticNextPhases
    for case List(
      PhaseTransition(curr1, next1),
      PhaseTransition(curr2, next2)
    ) <- progressCallback.progressPhasesFinal.sliding(2) do
      val curr1Index = indexOrFail(allPhasePlan, curr1)
      val curr2Index = indexOrFail(allPhasePlan, curr2)
      val next1Index = indexOrFail(allPhasePlan, next1)
      val next2Index = indexOrFail(allPhasePlan, next2)
      assertTrue(s"Phase `$curr1` did not come before `$curr2`", curr1Index < curr2Index)
      assertTrue(s"Phase `$next1` did not come before `$next2`", next1Index < next2Index)
      assertTrue(s"Phase `$curr1` did not come before `$next1`", curr1Index < next1Index)
      assertTrue(s"Phase `$curr2` did not come before `$next2`", curr2Index < next2Index)
      assertTrue(s"Predicted next phase `$next1` didn't match the following current `$curr2`", next1Index == curr2Index)

  /** Assert that the recorded progression of phases contains every phase in the plan */
  def assertFullCoverage(progressCallback: TestProgressCallback)(using Context): Unit =
    val (allPhasePlan, expectedCurrPhases, expectedNextPhases) =
      val allPhases = ctx.base.allPhases.flatMap(asSubphases)
      val firstPhase = allPhases.head
      val expectedCurrPhases = allPhases.toSet
      val expectedNextPhases = expectedCurrPhases - firstPhase ++ syntheticNextPhases
      (allPhases.toList, expectedCurrPhases, expectedNextPhases)

    for (expectedCurr, recordedCurr) <- allPhasePlan.zip(progressCallback.progressPhasesFinal.map(_.curr)) do
      assertEquals(s"Phase $recordedCurr was not expected", expectedCurr, recordedCurr)

    val (seenCurrPhases, seenNextPhases) =
      val (currs0, nexts0) = progressCallback.progressPhasesFinal.unzip(Tuple.fromProductTyped)
      (currs0.toSet, nexts0.toSet)

    val missingCurrPhases = expectedCurrPhases.diff(seenCurrPhases)
    val extraCurrPhases = seenCurrPhases.diff(expectedCurrPhases)
    assertTrue(s"these phases were not visited ${missingCurrPhases}", missingCurrPhases.isEmpty)
    assertTrue(s"these phases were visited, but not in the real plan ${extraCurrPhases}", extraCurrPhases.isEmpty)

    val missingNextPhases = expectedNextPhases.diff(seenNextPhases)
    val extraNextPhases = seenNextPhases.diff(expectedNextPhases)
    assertTrue(s"these phases were not planned to visit, but were expected ${missingNextPhases}", missingNextPhases.isEmpty)
    assertTrue(s"these phases were planned to visit, but were not in the real plan ${extraNextPhases}", extraNextPhases.isEmpty)


  /** Assert that the phases recorded per unit match the actual phases ran on them */
  def assertExpectedPhases(progressCallback: TestProgressCallback)(using Context): Unit =
    val expectedPhases = runnablePhases().flatMap(asSubphases)
    for (unit, visitedPhases) <- progressCallback.unitPhases do
      val uniquePhases = visitedPhases.toSet
      assert(unit != NoCompilationUnit, s"unexpected NoCompilationUnit for phases $uniquePhases")
      val duplicatePhases = visitedPhases.view.groupBy(identity).values.filter(_.size > 1).map(_.head)
      assertEquals(s"some phases were visited twice for $unit! ${duplicatePhases.toList}", visitedPhases.size, uniquePhases.size)
      val unvisitedPhases = expectedPhases.filterNot(visitedPhases.contains)
      val extraPhases = visitedPhases.filterNot(expectedPhases.contains)
      assertTrue(s"these phases were not visited for $unit ${unvisitedPhases}", unvisitedPhases.isEmpty)
      assertTrue(s"these phases were visited for $unit, but not expected ${extraPhases}", extraPhases.isEmpty)

  /** Assert that the number of total units of work matches the number of files * the runnable phases */
  def assertTotalUnits(progressCallback: TestProgressCallback)(using Context): Unit =
    var fileTraversals = 0 // files * phases
    for (_, phases) <- progressCallback.unitPhases do
      fileTraversals += phases.size
    val expectedTotal = fileTraversals // assume that no late enters occur
    progressCallback.totalEvents match
      case Nil => fail("No total events recorded")
      case TotalEvent(total, _) :: _ =>
        assertEquals(expectedTotal, total)

  def inspectProgress(sources: List[String], terminalPhase: Option[String] = Some("typer"))(op: Context ?=> TestProgressCallback => Unit) =
    // given Context = getCtx
    val sources0 = sources.map(_.linesIterator.map(_.trim.nn).filterNot(_.isEmpty).mkString("\n|").stripMargin)
    val terminalPhase0 = terminalPhase.getOrElse(defaultCompiler.phases.last.last.phaseName)
    checkAfterCompile(terminalPhase0, sources0) { case given Context =>
      ctx.progressCallback match
        case cb: TestProgressCallback => op(cb)
        case _ =>
          fail(s"Expected TestProgressCallback but got ${ctx.progressCallback}")
          ???
    }

  override protected def initializeCtx(fc: FreshContext): Unit =
    super.initializeCtx(
      fc.setProgressCallback(TestProgressCallback())
        .setSetting(fc.settings.outputDir, new VirtualDirectory("<TestProgressCallbackOutput>"))
    )

object ProgressCallbackTest:

  case class TotalEvent(total: Int, atPhase: String)
  case class ProgressEvent(curr: Int, total: Int, currPhase: String, nextPhase: String)
  case class PhaseTransition(curr: String, next: String)

  def asSubphases(phase: Phase): IndexedSeq[String] =
    val subPhases = Run.SubPhases(phase)
    val indices = 0 until phase.traversals
    indices.map(subPhases.subPhase)

  def runnablePhases()(using Context): IArray[Phase] =
    IArray.from(ctx.base.allPhases.filter(_.isRunnable))

  private val syntheticNextPhases = List("<end>")

  /** Asserts that the computed phase name exists in the real phase plan */
  def indexOrFail(allPhasePlan: Array[String], phaseName: String): Int =
    val i = allPhasePlan.indexOf(phaseName)
    if i < 0 then
      fail(s"Phase $phaseName not found")
    i

  final class TestProgressCallback extends interfaces.ProgressCallback:
    private var _cancelled: Boolean = false
    private var _unitPhases: Map[CompilationUnit, List[String]] = Map.empty
    private var _totalEvents: List[TotalEvent] = List.empty
    private var _progressPhases: List[PhaseTransition] = List.empty
    private var _shouldCancelNow: TestProgressCallback => Boolean = _ => false

    def totalEvents = _totalEvents
    def unitPhases = _unitPhases
    def progressPhasesFinal = _progressPhases.reverse

    def withCancelNow(f: TestProgressCallback => Boolean): this.type =
      _shouldCancelNow = f
      this

    override def cancel(): Unit = _cancelled = true
    override def isCancelled(): Boolean = _cancelled

    override def informUnitStarting(phase: String, unit: CompilationUnit): Unit =
      _unitPhases += (unit -> (unitPhases.getOrElse(unit, Nil) :+ phase))

    override def progress(current: Int, total: Int, currPhase: String, nextPhase: String): Boolean =
      // record the total and current phase whenever the total changes
      _totalEvents = _totalEvents match
        case Nil => TotalEvent(total, currPhase) :: Nil
        case events @ (head :: _) if head.total != total => TotalEvent(total, currPhase) :: events
        case events => events

      // record the current and next phase whenever the current phase changes
      _progressPhases = _progressPhases match
        case all @ PhaseTransition(head, _) :: rest =>
          if head != currPhase then
            PhaseTransition(currPhase, nextPhase) :: all
          else
            all
        case Nil => PhaseTransition(currPhase, nextPhase) :: Nil

      !_shouldCancelNow(this)

end ProgressCallbackTest
