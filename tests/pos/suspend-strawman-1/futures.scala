import collection.mutable.ListBuffer
import scala.util.boundary, boundary.{Label, break}
import runtime.*

/** A suspension and a value indicating the Future for which the suspension is waiting */
type Waiting = (Suspension[Unit], Future[?])

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
      suspend((s: Suspension[Unit]) => (s, this))
      await

object Future:

  private def complete[T](f: Future[T], value: T): Unit =
    f.result = Some(value)
    f.waiting.foreach(Scheduler.schedule)
    f.waiting = ListBuffer()

  /** Create future that eventually returns the result of `op` */
  def apply[T](body: CanWait ?=> T): Future[T] =
    val f = new Future[T]
    Scheduler.schedule: () =>
      boundary[Unit | Waiting]:
        complete(f, body)
      match
        case (suspension, blocking) =>
          blocking.waiting += (() => suspension.resume())
        case () =>
    f

def Test(x: Future[Int], xs: List[Future[Int]]) =
  Future:
    x.await + xs.map(_.await).sum







