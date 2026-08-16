package dotty.tools.dotc.core

import java.io.IOException

import dotty.tools.DottyTest
import dotty.tools.dotc.reporting.Diagnostic.LoadingError
import dotty.tools.dotc.reporting.StoreReporter

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

  private def loadingErrors(reporter: StoreReporter)(using Context): List[LoadingError] =
    reporter.removeBufferedMessages.collect:
      case error: LoadingError => error

  @Test def `failed load is cached and replayed once per reporter and run`(): Unit =
    val firstReporter = new StoreReporter(null)
    ctx = ctx.fresh.setReporter(firstReporter)
    val loader = new FailingLoader("broken binary")
    val (cls, moduleClass) = enterClassAndCompanion(newOwner("FailureOwner"), "Broken", loader)
    val sourceModule = moduleClass.sourceModule

    assertFalse(cls.isAbsent(canForce = false))
    assertEquals(0, loader.attempts)
    assertTrue(loadingErrors(firstReporter).isEmpty)

    assertTrue(cls.isAbsent())
    assertFalse(firstReporter.hasStickyErrors)
    val initialErrors = loadingErrors(firstReporter)
    assertEquals(1, loader.attempts)
    assertEquals(1, initialErrors.size)
    assertSame(
      ctx.base.loadingFailures(cls),
      ctx.base.loadingFailures(moduleClass)
    )

    assertTrue(cls.isAbsent())
    assertTrue(moduleClass.isAbsent())
    assertTrue(loadingErrors(firstReporter).isEmpty)

    ctx = ctx.fresh.setPeriod(Periods.Period(ctx.runId + 1, ctx.phaseId))
    assertTrue(cls.isAbsent())
    val nextRunErrors = loadingErrors(firstReporter)
    assertEquals(1, nextRunErrors.size)
    assertSame(initialErrors.head.failure, nextRunErrors.head.failure)

    ctx.base.reset()
    val replayReporter = new StoreReporter(null)
    ctx = ctx.fresh.setReporter(replayReporter)
    assertTrue(sourceModule.isAbsent(canForce = false))
    assertTrue(loadingErrors(replayReporter).isEmpty)

    assertTrue(sourceModule.isAbsent())
    assertTrue(cls.isAbsent())
    val replayedErrors = loadingErrors(replayReporter)
    assertEquals(1, replayedErrors.size)
    assertSame(initialErrors.head.failure, replayedErrors.head.failure)
    assertEquals(1, loader.attempts)

  @Test def `distinct same-message failures are both reported`(): Unit =
    val reporter = new StoreReporter(null)
    ctx = ctx.fresh.setReporter(reporter)
    val firstLoader = new FailingLoader("same failure")
    val secondLoader = new FailingLoader("same failure")
    val (firstClass, _) = enterClassAndCompanion(newOwner("FirstOwner"), "Broken", firstLoader)
    val (secondClass, _) = enterClassAndCompanion(newOwner("SecondOwner"), "Broken", secondLoader)

    assertTrue(firstClass.isAbsent())
    assertTrue(secondClass.isAbsent())
    val errors = loadingErrors(reporter)
    assertEquals(2, errors.size)
    assertEquals(errors.head.message, errors.last.message)
    assertNotSame(errors.head.failure, errors.last.failure)
    assertEquals(1, firstLoader.attempts)
    assertEquals(1, secondLoader.attempts)
