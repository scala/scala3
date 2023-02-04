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

  private var status: Status = Started
  private var result: Try[T] = uninitialized
  private var waiting: ListBuffer[Try[T] => Unit] = ListBuffer()

  private def addWaiting(k: Try[T] => Unit): Unit =
    waiting += k

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
            f.start()
            await(f)
          case Started   =>
            suspend[Try[T], Unit](s => f.addWaiting(s.resume))
          case Completed =>
            f.result
      body

  private def complete(): Unit =
    async:
      val result =
        try Success(body)
        catch case ex: Exception => Failure(ex)
      status = Completed
      for k <- waiting do
        Scheduler.schedule(() => k(result))
      waiting.clear()

  /** Start future's execution */
  def start(): this.type =
    if status == Initial then
      Scheduler.schedule(() => complete())
      status = Started
    this

object Future:
  enum Status:
    case Initial, Started, Completed
end Future

def Test(x: Future[Int], xs: List[Future[Int]]) =
  Future:
    x.await + xs.map(_.await).sum
  .start()








