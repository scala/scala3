import scala.collection.mutable.ArrayBuffer
import scala.util.chaining._

/** A wrapper for a list of cached instances of a type `T`.
  * The wrapper is recursion-reentrant: several instances are kept, so
  * at each depth of reentrance we are reusing the instance for that.
  *
  * An instance is created upon creating this object, and more instances
  * are allocated dynamically, on demand, when reentrance occurs.
  *
  * Not thread safe.
  *
  * Ported from scala.reflect.internal.util.ReusableInstance
  */
final class ReusableInstance[T <: AnyRef] private (make: => T) {
  private[this] val cache = new ArrayBuffer[T](ReusableInstance.InitialSize).tap(_.addOne(make))
  private[this] var taken = 0

  inline def withInstance[R](action: T => R): R ={
    if (taken == cache.size)
      cache += make
    taken += 1
    try action(cache(taken-1)) finally taken -= 1
  }
}

object ReusableInstance {
  private inline val InitialSize = 4

  def apply[T <: AnyRef](make: => T): ReusableInstance[T] = new ReusableInstance[T](make)
}

object Test extends App:
  val ri = ReusableInstance[String]("hi")
  ri.withInstance(x => x + x)
