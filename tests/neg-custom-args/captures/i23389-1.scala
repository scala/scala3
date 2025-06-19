import language.experimental.captureChecking
import caps.*

trait Collection[+T] extends Mutable:
  update def add(elem: T): Unit // error
  update def remove(elem: T): Unit // error
  def get(index: Int): Option[T]

