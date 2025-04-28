object outer:
  opaque type Queue = Queue.Unsafe
  object Queue:
    abstract class Unsafe
    opaque type Unbounded = Queue
    object Unbounded:
      inline def initWith()(f: Unbounded => Unit): Unit =
        f(Unsafe.init())

      opaque type Unsafe <: Queue.Unsafe = Queue
      object Unsafe:
        def init[A](): Unsafe = ???

object Resource:
  def run: Unit =
    outer.Queue.Unbounded.initWith() { q =>
      ???
    }
