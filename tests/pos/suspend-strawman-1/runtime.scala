import scala.util.boundary, boundary.Label
object runtime:

  /** A hypthetical task scheduler */
  object Scheduler:
    def schedule(task: Runnable): Unit = ???

  /** Contains a delimited contination, which can be invoked with `resume` */
  class Suspension[+R]:
    def resume(): R = ???

  /** Returns `fn(s)` where `s` is the current suspension to the boundary associated
   *  with the given label.
   */
  def suspend[R, T](fn: Suspension[R] => T)(using Label[T]): T = ???

  /** Returns the current suspension to the boundary associated
   *  with the given label.
   */
  def suspend[R]()(using Label[Suspension[R]]): Unit =
    suspend[R, Suspension[R]](identity)