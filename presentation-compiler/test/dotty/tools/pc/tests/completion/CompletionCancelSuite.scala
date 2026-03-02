package dotty.tools.pc.tests.completion

import java.lang
import java.net.URI
import java.util.concurrent.{CancellationException, CompletableFuture, CompletionStage}
import java.util.concurrent.atomic.AtomicBoolean

import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.{CompilerOffsetParams, EmptyCancelToken}
import scala.meta.internal.pc.{InterruptException, PresentationCompilerConfigImpl}
import scala.meta.pc.{CancelToken, PresentationCompilerConfig}

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionCancelSuite extends BaseCompletionSuite:

  override def config: PresentationCompilerConfigImpl =
    if isDebug then super.config
    else
      super.config.copy(
        // to make "break-compilation" test faster when run outside a debugger
        timeoutDelay = 5
      )

  /** A cancel token that cancels asynchronously on first `checkCancelled` call.
   */
  class AlwaysCancelToken extends CancelToken:
    val cancel = new CompletableFuture[lang.Boolean]()
    var isCancelled = new AtomicBoolean(false)
    override def onCancel(): CompletionStage[lang.Boolean] = cancel
    override def checkCanceled(): Unit =
      if isCancelled.compareAndSet(false, true) then
        cancel.complete(true)
      else
        Thread.sleep(10)

  def checkCancelled(
      query: String,
      expected: String
  ): Unit =
    val (code, offset) = params(query)
    val token = new AlwaysCancelToken
    try
      presentationCompiler
        .complete(
          CompilerOffsetParams(
            URI.create("file:///A.scala"),
            code,
            offset,
            token
          )
        )
        .get()
      fail("Expected completion request to be interrupted")
    catch
      case InterruptException() =>
        assert(token.isCancelled.get())

    // assert that regular completion works as expected.
    val completion = presentationCompiler
      .complete(
        CompilerOffsetParams(
          URI.create("file:///A.scala"),
          code,
          offset,
          EmptyCancelToken
        )
      )
      .get()
    val obtained = completion.getItems.asScala
      .map(_.getLabel)
      .sorted
      .mkString("\n")

    assertNoDiff(expected, obtained)

  @Test def `basic` =
    checkCancelled(
      """
        |object A {
        |  val x = asser@@
        |}
      """.stripMargin,
      """|assert(inline assertion: Boolean): Unit
         |assert(inline assertion: Boolean, inline message: => Any): Unit
         |""".stripMargin
    )

  /** A cancel token to simulate infinite compilation
   */
  object FreezeCancelToken extends CancelToken:
    val cancel = new CompletableFuture[lang.Boolean]()
    var isCancelled = new AtomicBoolean(false)
    override def onCancel(): CompletionStage[lang.Boolean] = cancel
    override def checkCanceled(): Unit =
      var hello = true
      var i = 0
      while hello do i += 1
      hello = false

  @Test def `break-compilation` =
    val query = """
                  |object A {
                  |  val x = asser@@
                  |}
               """.stripMargin
    val (code, offset) = params(query)
    val uri = URI.create("file:///A.scala")
    try
      presentationCompiler
        .complete(
          CompilerOffsetParams(
            uri,
            code,
            offset,
            FreezeCancelToken
          )
        )
        .get()
    catch case _: CancellationException => ()

    val res = presentationCompiler
      .complete(
        CompilerOffsetParams(
          uri,
          code,
          offset,
          EmptyCancelToken
        )
      )
      .get()
    assert(res.getItems().asScala.nonEmpty)
