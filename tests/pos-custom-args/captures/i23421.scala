import language.experimental.captureChecking
import caps.*

trait Collection[T] extends Mutable:
  update def add(elem: T): Unit
  update def remove(elem: T): Unit
  def get(index: Int): Option[T]

object Collection:
  def empty[T]: Collection[T] = ???

trait Foo:
  val thunks: Collection[() => Unit] // that's fine

class FooImpl1 extends Foo:
  val thunks: Collection[() => Unit] = Collection.empty // was error, now ok
