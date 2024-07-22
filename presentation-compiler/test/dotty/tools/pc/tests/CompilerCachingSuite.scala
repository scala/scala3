package dotty.tools.pc.tests

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.pc.base.BasePCSuite
import dotty.tools.pc.ScalaPresentationCompiler
import org.junit.{Before, Test}

import scala.language.unsafeNulls
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.pc.OffsetParams
import scala.concurrent.Future
import scala.concurrent.Await
import scala.meta.pc.VirtualFileParams
import scala.concurrent.duration.*

import java.util.Collections
import java.nio.file.Paths
import java.util.concurrent.CompletableFuture


class CompilerCachingSuite extends BasePCSuite:

  val timeout = 5.seconds

  private def checkCompilationCount(params: VirtualFileParams, expected: Int): Unit =
    presentationCompiler match
      case pc: ScalaPresentationCompiler =>
        val compilations= pc.compilerAccess.withNonInterruptableCompiler(Some(params))(-1, EmptyCancelToken) { driver =>
          driver.compiler().currentCtx.runId
        }.get(timeout.length, timeout.unit)
        assertEquals(expected, compilations, s"Expected $expected compilations but got $compilations")
      case _ => throw IllegalStateException("Presentation compiler should always be of type of ScalaPresentationCompiler")

  private def getContext(params: VirtualFileParams): Context =
    presentationCompiler match
      case pc: ScalaPresentationCompiler =>
        pc.compilerAccess.withNonInterruptableCompiler(Some(params))(null, EmptyCancelToken) { driver =>
          driver.compiler().currentCtx
        }.get(timeout.length, timeout.unit)
      case _ => throw IllegalStateException("Presentation compiler should always be of type of ScalaPresentationCompiler")

  @Before
  def beforeEach: Unit =
    presentationCompiler.restart()

    // We want to run art least one compilation, so runId points at 3.
    // This will ensure that we use the same driver, not recreate fresh one on each call
    val dryRunParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "dryRun", 1, EmptyCancelToken)
    checkCompilationCount(dryRunParams, 2)
    val freshContext = getContext(dryRunParams)
    presentationCompiler.complete(dryRunParams).get(timeout.length, timeout.unit)
    checkCompilationCount(dryRunParams, 3)
    val dryRunContext = getContext(dryRunParams)
    assert(freshContext != dryRunContext)


  @Test
  def `cursor-compilation-does-not-corrupt-cache`: Unit =

    val fakeParamsCursor = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = new", 15, EmptyCancelToken)
    val fakeParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = ne", 14, EmptyCancelToken)

    val contextPreCompilation = getContext(fakeParams)

    presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
    val contextPostFirst = getContext(fakeParams)
    assert(contextPreCompilation != contextPostFirst)
    checkCompilationCount(fakeParams, 4)

    presentationCompiler.complete(fakeParamsCursor).get(timeout.length, timeout.unit)
    val contextPostCursor = getContext(fakeParamsCursor)
    assert(contextPreCompilation != contextPostCursor)
    assert(contextPostFirst == contextPostCursor)
    checkCompilationCount(fakeParamsCursor, 4)

    presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
    val contextPostSecond = getContext(fakeParams)
    assert(contextPreCompilation != contextPostSecond)
    assert(contextPostFirst == contextPostCursor)
    assert(contextPostCursor == contextPostSecond)
    checkCompilationCount(fakeParamsCursor, 4)

  @Test
  def `compilation-for-same-snippet-is-cached`: Unit =
    val fakeParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = ne", 14, EmptyCancelToken)

    val contextPreCompilation = getContext(fakeParams)

    presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
    val contextPostFirst = getContext(fakeParams)
    assert(contextPreCompilation != contextPostFirst)
    checkCompilationCount(fakeParams, 4)

    presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
    val contextPostSecond = getContext(fakeParams)
    assert(contextPreCompilation != contextPostFirst)
    assert(contextPostSecond == contextPostFirst)
    checkCompilationCount(fakeParams, 4)

  @Test
  def `compilation-for-different-snippet-is-not-cached`: Unit =

    val fakeParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = prin", 16, EmptyCancelToken)
    val fakeParams2 = CompilerOffsetParams(Paths.get("Test2.scala").toUri(), "def hello = prin", 16, EmptyCancelToken)
    val fakeParams3 = CompilerOffsetParams(Paths.get("Test2.scala").toUri(), "def hello = print", 17, EmptyCancelToken)

    checkCompilationCount(fakeParams, 3)
    presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
    checkCompilationCount(fakeParams, 4)

    presentationCompiler.complete(fakeParams2).get(timeout.length, timeout.unit)
    checkCompilationCount(fakeParams2, 5)

    presentationCompiler.complete(fakeParams3).get(timeout.length, timeout.unit)
    checkCompilationCount(fakeParams3, 6)


  private val testFunctions: List[OffsetParams => CompletableFuture[_]] = List(
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
    val contextBefore = getContext(fakeParams)

    val differentContexts = testFunctions.map: f =>
      f(fakeParams).get(timeout.length, timeout.unit)
      checkCompilationCount(fakeParams, 4)
      getContext(fakeParams)
    .toSet

    assert(differentContexts == Set(contextBefore))

  @Test
  def `different-api-calls-reuse-cache-parallel`: Unit =
    import scala.jdk.FutureConverters.*
    import scala.concurrent.ExecutionContext.Implicits.global

    val fakeParams = CompilerOffsetParams(Paths.get("Test.scala").toUri(), "def hello = ne", 13, EmptyCancelToken)

    presentationCompiler.complete(fakeParams).get(timeout.length, timeout.unit)
    val contextBefore = getContext(fakeParams)

    val futures = testFunctions.map: f =>
      f(fakeParams).asScala.map(_ => getContext(fakeParams))

    val res = Await.result(Future.sequence(futures), timeout).toSet
    assert(res == Set(contextBefore))
