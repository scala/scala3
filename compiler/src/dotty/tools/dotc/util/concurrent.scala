package dotty.tools.dotc.util

import scala.util.{Try, Failure}
import scala.collection.mutable.ArrayBuffer

object concurrent:

  class NoCompletion extends RuntimeException

  class Future[T](exec: Executor[T]):
    private var result: Option[Try[T]] = None
    def force: Try[T] = synchronized {
      while result.isEmpty && exec.isAlive do wait(1000 /*ms*/)
      result.getOrElse(Failure(NoCompletion()))
    }
    def complete(r: Try[T]): Unit = synchronized {
      result = Some(r)
      notifyAll()
    }
  end Future

  class Executor[T] extends Thread:
    private type WorkItem = (Future[T], () => T)

    private var allScheduled = false
    private val pending = new ArrayBuffer[WorkItem]

    def schedule(op: () => T): Future[T] = synchronized {
      assert(!allScheduled)
      val f = Future[T](this)
      pending += ((f, op))
      notifyAll()
      f
    }

    def close(): Unit = synchronized {
      allScheduled = true
      notifyAll()
    }

    private def nextPending(): Option[WorkItem] = synchronized {
      while pending.isEmpty && !allScheduled do wait(1000 /*ms*/)
      if pending.isEmpty then None
      else
        val item = pending.head
        pending.dropInPlace(1)
        Some(item)
    }

    override def run(): Unit =
      while
        nextPending() match
          case Some((f, op)) =>
            f.complete(Try(op()))
            true
          case None =>
            false
      do ()
  end Executor
end concurrent



