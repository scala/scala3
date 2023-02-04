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
  import Future.Status, Status.*

  @volatile private var status: Status = Initial
  private var result: Try[T] = uninitialized
  private var waiting: ListBuffer[Try[T] => Unit] = ListBuffer()
  private var scheduler: Scheduler = uninitialized

  private def addWaiting(k: Try[T] => Unit): Unit = synchronized:
    if status == Completed then k(result)
    else waiting += k

  /** Wait for this future to be completed, return its value in case of success,
   *  or rethrow exception in case of failure.
   */
  def await(using a: Async): T = a.await(this).get

  // a handler for Async
  private def async(body: Async ?=> Unit): Unit =
    boundary [Unit]:
      given Async with
        def await[T](f: Future[T]): Try[T] = f.status match
          case Initial   =>
            f.start()(using scheduler)
            await(f)
          case Started   =>
            suspend[Try[T], Unit]: s =>
              f.addWaiting: result =>
                scheduler.schedule: () =>
                  s.resume(result)
          case Completed =>
            f.result
      body

  private def complete(): Unit =
    async:
      val result =
        try Success(body)
        catch case ex: Exception => Failure(ex)
      status = Completed
      waiting.foreach(_(result))
      waiting.clear()

  /** Start future's execution */
  def start()(using scheduler: Scheduler): this.type =
    this.scheduler = scheduler
    synchronized:
      if status == Initial then
        scheduler.schedule(() => complete())
        status = Started
    this

object Future:
  enum Status:
    case Initial, Started, Completed
end Future

def Test(x: Future[Int], xs: List[Future[Int]])(using Scheduler) =
  Future:
    x.await + xs.map(_.await).sum
  .start()








