package runtime
import scala.util.boundary, boundary.Label

/** A hypothetical task scheduler trait */
trait Scheduler:
  def schedule(task: Runnable): Unit = ???

object Scheduler extends Scheduler

/** Contains a delimited contination, which can be invoked with `resume` */
class Suspension[-T, +R]:
  def resume(arg: T): R = ???

def suspend[T, R](body: Suspension[T, R] => R)(using Label[R]): T = ???
