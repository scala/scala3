import scala.collection.mutable.ListBuffer
import scala.util.boundary, boundary.{Label, break}
import scala.annotation.unchecked.uncheckedVariance

// Alternative version of Futures following the design of @b-studios

/** A hypthetical task scheduler */
object Scheduler:
  def schedule(task: Runnable): Unit = ???

/** Contains a delimited contination, which can be invoked with `resume` */
class Suspension[+R]:
  def resume(): R = ???

def suspend[R](body: Suspension[R] => R)(using Label[R]): Unit = ???

trait Async:
  def await[T](f: Future[T]): T

class Future[+T] private():
  private var result: Option[T] = None
  private var waiting: ListBuffer[Runnable] = ListBuffer()
  def await(using a: Async): T = a.await(this)

object Future:
  private def complete[T](f: Future[T], value: T): Unit =
    f.result = Some(value)
    f.waiting.foreach(Scheduler.schedule)
    f.waiting = ListBuffer()

  // a handler for Async
  def async[T](body: Async ?=> Unit): Unit =
    boundary [Unit]:
      given Async with
        def await[T](f: Future[T]): T = f.result match
          case Some(x) => x
          case None =>
            suspend[Unit]: s =>
              f.waiting += (() => s.resume())
            f.await
      body

  def apply[T](body: Async ?=> T): Future[T] =
    val f = new Future[T]
    Scheduler.schedule: () =>
      async(complete(f, body))
    f

end Future

def Test(x: Future[Int], xs: List[Future[Int]])(using Async) =
  Future:
    x.await + xs.map(_.await).sum








