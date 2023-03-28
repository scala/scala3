package runtime
import scala.util.boundary, boundary.Label

/** A hypothetical API for suspensions. Not yet implemented.
 *  Suspension contain a delimited contination, which can be
 *  invoked with `resume`
 */
class Suspension[-T, +R]:
  def resume(arg: T): R = ???

def suspend[T, R](body: Suspension[T, R] => R)(using Label[R]): T = ???
