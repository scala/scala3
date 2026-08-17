package dotty.tools.dotc.core

import java.io.IOException
import scala.collection.mutable

import dotty.tools.DottyTest
import dotty.tools.dotc.reporting.Diagnostic.LoadingError
import dotty.tools.dotc.reporting.{ExploringReporter, StoreReporter}

import Contexts.*
import Flags.*
import Names.*
import Scopes.*
import Symbols.*
import org.junit.Assert.*
import org.junit.Test

class LoadingFailureTest extends DottyTest:

  private class FailingLoader(message: String) extends SymbolLoader:
    var attempts = 0

    def doComplete(root: SymDenotations.SymDenotation)(using Context): Unit =
      attempts += 1
      throw new IOException(message)

    def compilationUnitInfo: CompilationUnitInfo | Null = null

    def description(using Context): String = "failing test loader"

  private case class FailedLoad(
      loader: FailingLoader,
      cls: ClassSymbol,
      moduleClass: ClassSymbol
  )

  private def newOwner(name: String)(using Context): ClassSymbol =
    newCompleteClassSymbol(
      defn.EmptyPackageClass,
      typeName(name),
      EmptyFlags,
      defn.ObjectType :: Nil,
      newScope
    ).entered

  private def enterClassAndCompanion(
      owner: ClassSymbol,
      name: String,
      loader: SymbolLoader
  )(using Context): (ClassSymbol, ClassSymbol) =
    val term = termName(name)
    SymbolLoaders.enterClassAndModule(owner, term, loader)
    val cls = owner.unforcedDecls.lookup(term.toTypeName).asClass
    (cls, cls.scalacLinkedClass.asClass)

  private def newFailedLoad(ownerName: String, message: String = "broken binary")(using Context): FailedLoad =
    val loader = new FailingLoader(message)
    val (cls, moduleClass) = enterClassAndCompanion(newOwner(ownerName), "Broken", loader)
    FailedLoad(loader, cls, moduleClass)

  private def takeLoadingErrors(reporter: StoreReporter)(using Context): List[LoadingError] =
    reporter.removeBufferedMessages.map:
      case error: LoadingError => error
      case diagnostic =>
        throw new AssertionError(
          s"unexpected ${diagnostic.getClass.getSimpleName}: ${diagnostic.message}"
        )

  private def takeSingleLoadingError(reporter: StoreReporter)(using Context): LoadingError =
    val errors = takeLoadingErrors(reporter)
    assertEquals(1, errors.size)
    errors.head

  private def retainingContext(reporter: StoreReporter)(using Context): Context =
    val failures = ctx.property(RetainedSymbolLoadingFailures).getOrElse(mutable.WeakHashMap.empty)
    ctx.fresh
      .setReporter(reporter)
      .setProperty(RetainedSymbolLoadingFailures, failures)

  @Test def `failed loads are not retained without the context property`(): Unit =
    val initialReporter = new StoreReporter(null)
    ctx = ctx.fresh
      .addMode(Mode.Interactive)
      .setReporter(initialReporter)
      .dropProperty(RetainedSymbolLoadingFailures)
    val failed = newFailedLoad("UnretainedOwner")

    assertTrue(failed.cls.isAbsent())
    val initialDiagnostics = initialReporter.removeBufferedMessages
    assertEquals(1, initialDiagnostics.size)
    assertFalse(initialDiagnostics.head.isInstanceOf[LoadingError])
    assertTrue(ctx.property(RetainedSymbolLoadingFailures).isEmpty)

    val replayReporter = new StoreReporter(null)
    ctx = retainingContext(replayReporter)
    assertTrue(failed.cls.isAbsent())
    assertTrue(replayReporter.removeBufferedMessages.isEmpty)
    assertEquals(1, failed.loader.attempts)

  @Test def `retained failure is not replayed outside the retaining context`(): Unit =
    val initialReporter = new StoreReporter(null)
    ctx = retainingContext(initialReporter)
    val failed = newFailedLoad("DisabledReplayOwner")

    assertTrue(failed.cls.isAbsent())
    takeSingleLoadingError(initialReporter)

    val replayReporter = new StoreReporter(null)
    ctx = ctx.fresh
      .setReporter(replayReporter)
      .dropProperty(RetainedSymbolLoadingFailures)
    assertTrue(failed.cls.isAbsent())
    assertTrue(replayReporter.removeBufferedMessages.isEmpty)
    assertEquals(1, failed.loader.attempts)

  @Test def `forcing a failed load reports a non-sticky error`(): Unit =
    val reporter = new StoreReporter(null)
    ctx = retainingContext(reporter)
    val failed = newFailedLoad("NonStickyOwner")

    assertFalse(failed.cls.isAbsent(canForce = false))
    assertEquals(0, failed.loader.attempts)
    assertTrue(takeLoadingErrors(reporter).isEmpty)

    assertTrue(failed.cls.isAbsent())
    takeSingleLoadingError(reporter)
    assertFalse(reporter.hasStickyErrors)
    assertEquals(1, failed.loader.attempts)

  @Test def `failed load is attempted once for a class and its companion`(): Unit =
    val reporter = new StoreReporter(null)
    ctx = retainingContext(reporter)
    val failed = newFailedLoad("SingleAttemptOwner")

    assertTrue(failed.cls.isAbsent())
    takeSingleLoadingError(reporter)

    assertTrue(failed.cls.isAbsent())
    assertTrue(failed.moduleClass.isAbsent())
    assertTrue(takeLoadingErrors(reporter).isEmpty)
    assertEquals(1, failed.loader.attempts)

  @Test def `class and companion replay the same load failure`(): Unit =
    val classReporter = new StoreReporter(null)
    ctx = retainingContext(classReporter)
    val failed = newFailedLoad("CompanionOwner")

    assertTrue(failed.cls.isAbsent())
    val classError = takeSingleLoadingError(classReporter)

    val companionReporter = new StoreReporter(null)
    ctx = retainingContext(companionReporter)
    assertTrue(failed.moduleClass.isAbsent())
    val companionError = takeSingleLoadingError(companionReporter)
    assertSame(classError.failure, companionError.failure)
    assertEquals(1, failed.loader.attempts)

  @Test def `failed load is reported again in a later run`(): Unit =
    val reporter = new StoreReporter(null)
    ctx = retainingContext(reporter)
    val failed = newFailedLoad("LaterRunOwner")

    assertTrue(failed.cls.isAbsent())
    val initialError = takeSingleLoadingError(reporter)

    ctx = ctx.fresh.setPeriod(Periods.Period(ctx.runId + 1, ctx.phaseId))
    assertTrue(failed.cls.isAbsent())
    val nextRunError = takeSingleLoadingError(reporter)
    assertSame(initialError.failure, nextRunError.failure)
    assertEquals(1, failed.loader.attempts)

  @Test def `failed load survives a context base reset`(): Unit =
    val initialReporter = new StoreReporter(null)
    ctx = retainingContext(initialReporter)
    val failed = newFailedLoad("ResetOwner")

    assertTrue(failed.cls.isAbsent())
    val initialError = takeSingleLoadingError(initialReporter)

    ctx.base.reset()
    val replayReporter = new StoreReporter(null)
    ctx = ctx.fresh.setReporter(replayReporter)
    assertTrue(failed.moduleClass.isAbsent(canForce = false))
    assertTrue(takeLoadingErrors(replayReporter).isEmpty)

    assertTrue(failed.moduleClass.isAbsent())
    val replayedError = takeSingleLoadingError(replayReporter)
    assertSame(initialError.failure, replayedError.failure)
    assertEquals(1, failed.loader.attempts)

  @Test def `markAbsent preserves an existing load failure`(): Unit =
    val initialReporter = new StoreReporter(null)
    ctx = retainingContext(initialReporter)
    val failed = newFailedLoad("AbsentOwner")

    assertTrue(failed.cls.isAbsent())
    val initialError = takeSingleLoadingError(initialReporter)
    failed.cls.denot.markAbsent()

    val replayReporter = new StoreReporter(null)
    ctx = retainingContext(replayReporter)
    assertTrue(failed.cls.isAbsent())
    val replayedError = takeSingleLoadingError(replayReporter)
    assertSame(initialError.failure, replayedError.failure)

  @Test def `exploring reporter can report a failure again after reset`(): Unit =
    val reporter = new ExploringReporter
    ctx = retainingContext(reporter)
    val failed = newFailedLoad("ExploringOwner")

    assertTrue(failed.cls.isAbsent())
    assertEquals(1, reporter.pendingMessages.size)
    reporter.reset()

    assertTrue(failed.cls.isAbsent())
    takeSingleLoadingError(reporter)
    assertEquals(1, failed.loader.attempts)

  @Test def `distinct same-message failures are both reported`(): Unit =
    val reporter = new StoreReporter(null)
    ctx = retainingContext(reporter)
    val first = newFailedLoad("FirstOwner", "same failure")
    val second = newFailedLoad("SecondOwner", "same failure")

    assertTrue(first.cls.isAbsent())
    assertTrue(second.cls.isAbsent())
    val errors = takeLoadingErrors(reporter)
    assertEquals(2, errors.size)
    assertNotSame(errors.head.failure, errors.last.failure)
    assertEquals(1, first.loader.attempts)
    assertEquals(1, second.loader.attempts)
