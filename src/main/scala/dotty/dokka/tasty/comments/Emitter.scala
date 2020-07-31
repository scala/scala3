package dotty.dokka.tasty.comments

import scala.collection.mutable.ArrayBuffer

object Emitter {
  opaque type Emitter[T] = ArrayBuffer[T]

  def collect[T](thunk: Emitter[T] ?=> Unit): Seq[T] = {
    val bld = new ArrayBuffer[T]
    thunk(using bld)
    bld.toSeq
  }

  def emit[T](using e: Emitter[T])(t: T) = e.addOne(t)

  def lastEmittedItem[T](using e: Emitter[T]) =
    if e.isEmpty then None else Some(e.last)
}
