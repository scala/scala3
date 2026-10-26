package scala.collection
package immutable

import java.util.concurrent.{CountDownLatch, TimeUnit}

import org.junit.Assert.*
import org.junit.Test

import scala.annotation.nowarn

/** A thunk that throws an `Error` must leave the list re-evaluable, and must release threads
 *  waiting on it (`initState` catches `Throwable`, not just `Exception`).
 */
@nowarn("msg=Catching Throwable")
class LazyListThrowableTest {
  private def forceOnOtherThread(force: () => Any): Throwable | Null = {
    @volatile var result: Throwable | Null = null
    val done = new CountDownLatch(1)
    val t = new Thread(() => {
      try force() catch { case e: Throwable => result = e }
      done.countDown()
    })
    t.setDaemon(true)
    t.start()
    assertTrue("forcing thread blocked on a failed LazyList thunk", done.await(10, TimeUnit.SECONDS))
    result
  }

  @Test def lazyListErrorRethrownOnEveryForce(): Unit = {
    val rows = "header" #:: (??? : LazyList[String])
    assertThrows(classOf[NotImplementedError], () => rows.toList)
    assertThrows(classOf[NotImplementedError], () => rows.toList)
    assertTrue(forceOnOtherThread(() => rows.toList).isInstanceOf[NotImplementedError])
  }

  @Test def lazyListIterableErrorRethrownOnEveryForce(): Unit = {
    val rows = "header" #:: (??? : LazyListIterable[String])
    assertThrows(classOf[NotImplementedError], () => rows.toList)
    assertThrows(classOf[NotImplementedError], () => rows.toList)
    assertTrue(forceOnOtherThread(() => rows.toList).isInstanceOf[NotImplementedError])
  }

  @Test def lazyListRetryableAfterStackOverflow(): Unit = {
    def walk(n: Int): Int = if (n == 0) 0 else 1 + walk(n - 1)
    var limit = 50000000
    val l = 1 #:: { LazyList(walk(limit)) }
    assertThrows(classOf[StackOverflowError], () => l.toList)
    limit = 1000
    assertEquals(List(1, 1000), l.toList)
    assertEquals(List(1, 1000), l.toList)
  }

  private def assertThrows(c: Class[? <: Throwable], f: () => Any): Unit =
    try { f(); fail(s"expected $c") } catch { case e: Throwable if c.isInstance(e) => }
}
