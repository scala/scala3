import language.experimental.captureChecking
import caps.*

trait Collection[T] extends Mutable // <- note the forgotten :
  update def add(elem: T): Unit // error // error
  update def remove(elem: T): Unit // error // error
  def get(index: Int): Option[T] // error // error

