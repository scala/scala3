import language.experimental.safe
import caps.unsafe.untrackedCaptures
import caps.assumeSafe
import scala.collection.mutable.HashMap

@assumeSafe // error
class Memoized[A, B](f: A -> B) {

  @untrackedCaptures
  private val cached = HashMap[A, B]()

  val x: A => B = f.asInstanceOf[A => B]

  def apply(x: A) = cached.getOrElseUpdate(x, f(x))
}