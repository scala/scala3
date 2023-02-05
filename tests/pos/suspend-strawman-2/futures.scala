package futures

import scala.collection.mutable.ListBuffer
import scala.util.boundary, boundary.Label
import scala.compiletime.uninitialized
import scala.util.{Try, Success, Failure}
import runtime.*

trait Async:

  /** Wait for completion of future `f`. This means:
   *   - ensure that computing `f` has started
   *   - wait for the completion and return the completed Try
   */
  def await[T](f: Future[T]): Try[T]

end Async

class Future[+T](body: Async ?=> T):
  import Future.{Status, Cancellation}, Status.*

  @volatile private var status: Status = Initial
  private var result: Try[T] = uninitialized
  private var waiting: ListBuffer[Try[T] => Unit] = ListBuffer()
  private var scheduler: Scheduler = uninitialized

  private def addWaiting(k: Try[T] => Unit): Unit = synchronized:
    if status == Completed then k(result)
    else waiting += k

  private def currentWaiting(): List[Try[T] => Unit] = synchronized:
    val ws = waiting.toList
    waiting.clear()
    ws

  private def checkCancellation(): Unit =
    if status == Cancelled then throw Cancellation()

  /** Wait for this future to be completed, return its value in case of success,
   *  or rethrow exception in case of failure.
   */
  def await(using a: Async): T = a.await(this).get

  // a handler for Async
  private def async(body: Async ?=> Unit): Unit =
    boundary [Unit]:
      given Async with

        private def resultOption[T](f: Future[T]): Option[Try[T]] = f.status match
          case Initial =>
            f.ensureStarted()(using scheduler)
            resultOption(f)
          case Started =>
            None
          case Completed =>
            Some(f.result)
          case Cancelled =>
            Some(Failure(Cancellation()))

        private inline def cancelChecked[T](op: => T): T =
          checkCancellation()
          val res = op
          checkCancellation()
          res

        def await[T](f: Future[T]): Try[T] =
          cancelChecked:
            resultOption(f).getOrElse:
              suspend[Try[T], Unit]: s =>
                f.addWaiting: result =>
                  scheduler.schedule: () =>
                    s.resume(result)
      body
  end async

  private def complete(): Unit =
    async:
      val result =
        try Success(body)
        catch case ex: Exception => Failure(ex)
      status = Completed
      for task <- currentWaiting() do task(result)

  /** Ensure future's execution has started */
  def ensureStarted()(using scheduler: Scheduler): this.type =
    synchronized:
      if status == Initial then start()
    this

  /** Start future's execution
   *  @pre future has not yet started
   */
  def start()(using scheduler: Scheduler): this.type =
    synchronized:
      assert(status == Initial)
      this.scheduler = scheduler
      scheduler.schedule(() => complete())
      status = Started
    this

  def cancel(): Unit = synchronized:
    if status != Completed && status != Cancelled then
      status = Cancelled

object Future:

  class Cancellation extends Exception

  enum Status:
    // Transitions always go left to right.
    // Cancelled --> Completed with Failure(Cancellation()) result
    case Initial, Started, Cancelled, Completed

  /** Construct a future and start it so that ion runs in parallel with the
   *  current thread.
   */
  def spawn[T](body: Async ?=> T)(using Scheduler): Future[T] =
    Future(body).start()
end Future

def Test(x: Future[Int], xs: List[Future[Int]])(using Scheduler): Future[Int] =
  Future.spawn:
    x.await + xs.map(_.await).sum
