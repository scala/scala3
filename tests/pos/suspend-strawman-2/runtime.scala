import scala.util.boundary, boundary.Label
object runtime:

  /** A hypthetical task scheduler */
  object Scheduler:
    def schedule(task: Runnable): Unit = ???

  /** Contains a delimited contination, which can be invoked with `resume` */
  class Suspension[-T, +R]:
    def resume(arg: T): R = ???

  def suspend[T, R](body: Suspension[T, R] => R)(using Label[R]): T = ???
