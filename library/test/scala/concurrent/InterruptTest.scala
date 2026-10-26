package scala.concurrent

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

import org.junit.Assert.*
import org.junit.Test

import scala.concurrent.duration.*
import scala.util.Failure

/** An interrupt (e.g. thread pool shutdown) of a thread running a transformation must not
 *  leave the derived future incomplete, nor drop the rest of a batch. These rely on
 *  `Promise.Transformation` and `BatchingExecutor.AsyncBatch` catching `Throwable`, not just `NonFatal`.
 */
class InterruptTest {
  private def await[T](f: Future[T]): scala.util.Try[T] = {
    Await.ready(f, 10.seconds)
    f.value.get
  }

  @Test def mapInterruptedCompletesFuture(): Unit = {
    val pool = Executors.newSingleThreadExecutor()
    try {
      given ExecutionContext = ExecutionContext.fromExecutor(pool)
      val started = new CountDownLatch(1)
      var worker: Thread | Null = null
      val job = Future("job").map { s =>
        worker = Thread.currentThread
        started.countDown()
        Thread.sleep(60000)
        s
      }
      var completed = false
      job.onComplete(_ => completed = true)
      assertTrue(started.await(10, TimeUnit.SECONDS))
      worker.nn.interrupt()
      assertTrue(await(job).isInstanceOf[Failure[?]])
      pool.shutdown()
      assertTrue(pool.awaitTermination(10, TimeUnit.SECONDS))
      assertTrue(completed)
    } finally pool.shutdownNow()
  }

  @Test def interruptedBatchedTransformationDoesNotDropBatch(): Unit = {
    val ec = ExecutionContext.opportunistic
    val p = Promise[Int]()
    val started = new CountDownLatch(1)
    @volatile var batchThread: Thread | Null = null
    // a batch runs in reverse registration order, so the blocking map goes first
    val blocked = p.future.map { i =>
      batchThread = Thread.currentThread
      started.countDown()
      Thread.sleep(60000)
      i
    }(using ec)
    val other1 = p.future.map(_ * 10)(using ec)
    val other2 = p.future.map(_ * 100)(using ec)
    // complete from a thread that is not itself batching, so the batch is submitted to the underlying pool
    p.success(42)
    assertTrue(started.await(10, TimeUnit.SECONDS))
    batchThread.nn.interrupt()
    assertTrue(await(blocked).isInstanceOf[Failure[?]])
    assertEquals(420, Await.result(other1, 10.seconds))
    assertEquals(4200, Await.result(other2, 10.seconds))
  }
}
