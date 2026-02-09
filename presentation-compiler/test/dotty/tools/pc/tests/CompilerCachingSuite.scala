package dotty.tools.pc.tests

import java.nio.file.Paths
import java.util.Collections
import java.util.concurrent.CompletableFuture

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.language.unsafeNulls
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.metals.PcQueryContext
import scala.meta.pc.OffsetParams
import scala.meta.pc.VirtualFileParams
import scala.meta.pc.reports.EmptyReportContext

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.pc.ScalaPresentationCompiler
import dotty.tools.pc.base.BasePCSuite

import org.junit.{Before, Test}

class CompilerCachingSuite extends BasePCSuite:

  val timeout = 5.seconds

  private def checkCompilationCount(expected: Int): Unit =
    presentationCompiler match
      case pc: ScalaPresentationCompiler =>
        val compilations = pc.compilerAccess.withNonInterruptableCompiler(-1, EmptyCancelToken) { driver =>
          driver.compiler().currentCtx.runId
        }(using emptyQueryContext).get(timeout.length, timeout.unit)
        assertEquals(expected, compilations, s"Expected $expected compilations but got $compilations")
      case _ =>
        throw IllegalStateException("Presentation compiler should always be of type of ScalaPresentationCompiler")

  private def getContext(): Context =
    presentationCompiler match
      case pc: ScalaPresentationCompiler =>
        pc.compilerAccess.withNonInterruptableCompiler(null, EmptyCancelToken) { driver =>
          driver.compiler().currentCtx
        }(using emptyQueryContext).get(timeout.length, timeout.unit)
      case _ =>
        throw IllegalStateException("Presentation compiler should always be of type of ScalaPresentationCompiler")

  private def emptyQueryContext = PcQueryContext(None, () => "")(using EmptyReportContext())

  @Before
  def beforeEach: Unit =
    presentationCompiler.restart()

    // We want to run at least one compilation, so runId points at 3.
    // This will ensure that we use the same driver, not recreate fresh one on each call
    val dryRunParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "dryRun", 1, EmptyCancelToken)
    checkCompilationCount(2)
    val freshContext = getContext()
    presentationCompiler.complete(dryRunParams).get(timeout.length, timeout.unit)
    checkCompilationCount(3)
    val dryRunContext = getContext()
    assert(freshContext != dryRunContext)

  @Test
  def `cursor-compilation-does-not-corrupt-cache`: Unit =
    val contextPreCompilation = getContext()

    val fakeParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = ne", 14, EmptyCancelToken)
    presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
    val contextPostFirst = getContext()
    assert(contextPreCompilation != contextPostFirst)
    checkCompilationCount(4)

    val fakeParamsCursor =
      CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = new", 15, EmptyCancelToken)
    presentationCompiler.complete(fakeParamsCursor).get(timeout.length, timeout.unit)
    val contextPostCursor = getContext()
    assert(contextPreCompilation != contextPostCursor)
    assert(contextPostFirst == contextPostCursor)
    checkCompilationCount(4)

    presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
    val contextPostSecond = getContext()
    assert(contextPreCompilation != contextPostSecond)
    assert(contextPostFirst == contextPostCursor)
    assert(contextPostCursor == contextPostSecond)
    checkCompilationCount(4)

  @Test
  def `dot-compilation-does-not-corrupt-cache`: Unit =
    val contextPreCompilation = getContext()

    val fakeParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = 1.", 14, EmptyCancelToken)
    presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
    val contextPostFirst = getContext()
    assert(contextPreCompilation != contextPostFirst)
    checkCompilationCount(4)

    presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
    val contextPostSecond = getContext()
    assert(contextPreCompilation != contextPostFirst)
    assert(contextPostSecond == contextPostFirst)
    checkCompilationCount(4)

  @Test
  def `compilation-for-same-snippet-is-cached`: Unit =
    val contextPreCompilation = getContext()

    val fakeParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = ne", 14, EmptyCancelToken)
    presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
    val contextPostFirst = getContext()
    assert(contextPreCompilation != contextPostFirst)
    checkCompilationCount(4)

    presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
    val contextPostSecond = getContext()
    assert(contextPreCompilation != contextPostFirst)
    assert(contextPostSecond == contextPostFirst)
    checkCompilationCount(4)

  @Test
  def `compilation-for-different-snippet-is-not-cached`: Unit =

    checkCompilationCount(3)
    val fakeParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = prin", 16, EmptyCancelToken)
    presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
    checkCompilationCount(4)

    val fakeParams2 = CompilerOffsetParams(Paths.get("Test2.scala").toUri(), "def hello = prin", 16, EmptyCancelToken)
    presentationCompiler.complete(fakeParams2).get(timeout.length, timeout.unit)
    checkCompilationCount(5)

    val fakeParams3 = CompilerOffsetParams(Paths.get("Test2.scala").toUri(), "def hello = print", 17, EmptyCancelToken)
    presentationCompiler.complete(fakeParams3).get(timeout.length, timeout.unit)
    checkCompilationCount(6)

  private val testFunctions: List[OffsetParams => CompletableFuture[?]] = List(
    presentationCompiler.complete(_),
    presentationCompiler.convertToNamedArguments(_, Collections.emptyList()),
    presentationCompiler.autoImports("a", _, false),
    presentationCompiler.definition(_),
    presentationCompiler.didChange(_),
    presentationCompiler.documentHighlight(_),
    presentationCompiler.hover(_),
    presentationCompiler.implementAbstractMembers(_),
    presentationCompiler.insertInferredType(_),
    presentationCompiler.semanticTokens(_),
    presentationCompiler.prepareRename(_),
    presentationCompiler.rename(_, "a"),
    presentationCompiler.signatureHelp(_),
    presentationCompiler.typeDefinition(_)
  )

  @Test
  def `different-api-calls-reuse-cache`: Unit =
    val fakeParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = ne", 13, EmptyCancelToken)
    presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)

    val contextBefore = getContext()

    val differentContexts = testFunctions.map: f =>
      f(fakeParams).get(timeout.length, timeout.unit)
      checkCompilationCount(4)
      getContext()
    .toSet

    assert(differentContexts == Set(contextBefore))

  @Test
  def `different-api-calls-reuse-cache-parallel`: Unit =
    import scala.jdk.FutureConverters.*
    import scala.concurrent.ExecutionContext.Implicits.global

    val fakeParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = ne", 13, EmptyCancelToken)
    presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)

    val contextBefore = getContext()

    val futures = testFunctions.map: f =>
      f(fakeParams).asScala.map(_ => getContext())

    val res = Await.result(Future.sequence(futures), timeout).toSet
    assert(res == Set(contextBefore))
