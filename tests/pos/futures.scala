import collection.mutable.ListBuffer
import scala.util.boundary, boundary.{Label, break}

/** A hypthetical task scheduler */
object Scheduler:
  def schedule(task: Runnable): Unit = ???

/** Contains a delimited contination, which can be invoked with `run`,
 *  plus some other value that is returned from a `suspend`.
 */
case class Suspension[+T](x: T):
  def resume(): Unit = ???

/** Returns `Suspension(x)` to the boundary associated with the given label */
def suspend[T](x: T)(using Label[Suspension[T]]): Unit =
  break(Suspension(x))

/** A suspension indicating the Future for which it is waiting */
type Waiting = Suspension[Future[?]]

/** The capability to suspend while waiting for some other future */
type CanWait = Label[Waiting]

class Future[+T] private ():
  private var result: Option[T] = None
  private var waiting: ListBuffer[Runnable] = ListBuffer()

  /** Return future's result, while waiting until it is completed if necessary.
   */
  def await(using CanWait): T = result match
    case Some(x) => x
    case None =>
      suspend(this)
      await

object Future:

  private def complete[T](f: Future[T], value: T): Unit =
    f.result = Some(value)
    f.waiting.foreach(Scheduler.schedule)
    f.waiting = ListBuffer()

  /** Create future that eventually returns the result of `op` */
  def apply[T](body: => T): Future[T] =
    val f = new Future[T]
    Scheduler.schedule: () =>
      boundary[Unit | Waiting]:
        complete(f, body)
      match
        case s @ Suspension(blocking) =>
          blocking.waiting += (() => s.resume())
        case () =>
    f

def Test(x1: Future[Int], x2: Future[Int])(using CanWait) =
  Future:
    x1.await + x2.await








