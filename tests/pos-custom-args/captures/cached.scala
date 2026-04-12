import caps.unsafe.untrackedCaptures
import caps.assumeSafe
import scala.collection.mutable.HashMap

@assumeSafe
class Memoized[A, B](f: A -> B) {

  @untrackedCaptures
  private val cached = HashMap[A, B]()

  def apply(x: A) = cached.getOrElseUpdate(x, f(x))
}
