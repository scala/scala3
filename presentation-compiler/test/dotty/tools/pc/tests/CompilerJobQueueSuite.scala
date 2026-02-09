package dotty.tools.pc.tests

import java.util.concurrent.CompletableFuture

import scala.collection.mutable
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.language.unsafeNulls
import scala.meta.internal.pc.CompilerJobQueue
import scala.util.Try

import org.junit.{After, Assert, Before, Test}

class CompilerJobQueueSuite:
  var jobs: CompilerJobQueue = null

  @Before
  def beforeEach: Unit =
    jobs = CompilerJobQueue()

  @After
  def afterEach: Unit =
    jobs.shutdown()

  @Test def `cancel` =
    val cancelled = new CompletableFuture[Unit]()
    cancelled.cancel(false)
    jobs.submit(
      cancelled,
      () => Thread.sleep(1000)
    )
    jobs.shutdown()
    assert(Try(cancelled.get).isFailure)

  @Test def `order` =
    val obtained = mutable.ListBuffer.empty[Int]
    val size = 10
    val original = 1.to(size).toList
    val all = Future.traverse(original) { i =>
      val promise = Promise[Unit]()
      jobs.submit(() =>
        Thread.sleep(i * 5)
        obtained += i
        promise.success(())
      )
      promise.future
    }

    Await.result(all, Duration("1s"))

    // Assert all submitted non-cancelled jobs completed.
    Assert.assertEquals(obtained.length, size)

    // Assert that the jobs don't run in the default order.
    Assert.assertNotEquals(obtained.toList, original)
