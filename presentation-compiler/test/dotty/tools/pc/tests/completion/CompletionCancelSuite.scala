package dotty.tools.pc.tests.completion

import java.lang
import java.net.URI
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{
  CancellationException,
  CompletableFuture,
  CompletionStage
}

import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.{CompilerOffsetParams, EmptyCancelToken}
import scala.meta.internal.pc.{
  InterruptException,
  PresentationCompilerConfigImpl
}
import scala.meta.pc.{CancelToken, PresentationCompilerConfig}
import scala.language.unsafeNulls

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test
import org.junit.Assert

class CompletionCancelSuite extends BaseCompletionSuite:

  override protected def config: PresentationCompilerConfig =
    PresentationCompilerConfigImpl().copy(
      // to make "break-compilation" test faster
      timeoutDelay = 5
    )

  /**
   * A cancel token that cancels asynchronously on first `checkCancelled` call.
   */
  class AlwaysCancelToken extends CancelToken:
    val cancel = new CompletableFuture[lang.Boolean]()
    override def onCancel(): CompletionStage[lang.Boolean] = cancel
    override def checkCanceled(): Unit =
      cancel.complete(true)
      throw new CancellationException("Always Cancel Token")

  /**
   * A cancel token that cancels after 5 `checkCancelled` calls.
   */
  class DelayedCancelToken extends CancelToken:
    val cancel = new CompletableFuture[lang.Boolean]()
    override def onCancel(): CompletionStage[lang.Boolean] = cancel
    var i = 0
    override def checkCanceled(): Unit =
      i += 1
      if i > 5 then
        cancel.complete(true)
        throw new CancellationException("Delayed Cancel Token")

  def checkCancelled(
      token: CancelToken,
      query: String,
      expected: String,
  ): Unit =
    val (code, offset) = params(query)
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
      case _ =>
        assert(token.onCancel().toCompletableFuture.get())

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

  @Test def `cancel-before-start` =
    checkCancelled(
      AlwaysCancelToken(),
      """
        |object A {
        |  val x = asser@@
        |}
      """.stripMargin,
      """|assert(inline assertion: Boolean): Unit
         |assert(inline assertion: Boolean, inline message: => Any): Unit
         |""".stripMargin
    )

  @Test def `cancel-during-compilation` =
    checkCancelled(
      DelayedCancelToken(),
      """
        |object A {
        |  val x = asse@@
        |}
      """.stripMargin,
      """|assert(inline assertion: Boolean): Unit
         |assert(inline assertion: Boolean, inline message: => Any): Unit
         |""".stripMargin
    )

  // With new assumptions we can't run this test without forking the JVM.
  // We now expect to handle infinite compilations at the call site.
  //
  // This is very dangerous and potentially will make users unresponsive,
  // and there is also no good way to handle infinite compilations.
  //
  // We will try to catch all of them, but right now the approach that
  // ensures no leaks is the way to go.
  //
  // In previous implementations this test was working, because we
  // didn't log, catch the infinite computations, we've just created new
  // presentation compiler driver and went straight back to work, while
  // this zombie thread was still stuck.
  //
  // /**
  //  * A cancel token to simulate infinite compilation
  //  */
  // object FreezeCancelToken extends CancelToken:
  //   val cancel = new CompletableFuture[lang.Boolean]()
  //   var isCancelled = new AtomicBoolean(false)
  //   var count = 0
  //   override def onCancel(): CompletionStage[lang.Boolean] = cancel
  //   override def checkCanceled(): Unit =
  //     var hello = false
  //     var i = 0
  //     count += 1
  //     if count > 2 then hello = true
  //     while (hello) i += 1
  //     hello = false

  // @Test def `zombie-task-detection` =
  //   val query = """
  //                 |object A {
  //                 |  val x = asser@@
  //                 |}
  //              """.stripMargin
  //   val (code, offset) = params(query)
  //   val uri = URI.create("file:///A.scala")

  //   Assert.assertThrows(classOf[InfiniteCompilationException], () =>
  //     try
  //       presentationCompiler
  //         .complete(
  //           CompilerOffsetParams(
  //             uri,
  //             code,
  //             offset,
  //             FreezeCancelToken
  //           )
  //         ).get()
  //     catch case _: CancellationException => ()
  //   )

  //   val res = presentationCompiler
  //     .complete(
  //       CompilerOffsetParams(
  //         uri,
  //         code,
  //         offset,
  //         EmptyCancelToken
  //       )
  //     )
  //     .get()
  //   assert(res.getItems().asScala.nonEmpty)
