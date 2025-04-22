object outer:
  opaque type Queue = Queue.Unsafe
  object Queue:
    abstract class Unsafe
    opaque type Unbounded = Queue
    object Unbounded:
      opaque type Unsafe <: Queue.Unsafe = Queue
      object Unsafe:
        def init[A](): Unsafe = ???

object Resource:
  def run: Unit =
    val $proxy2: outer.type{type Queue = outer.Queue.Unsafe} =
      outer.asInstanceOf[outer.type{type Queue = outer.Queue.Unsafe}]
    val $proxy1: $proxy2.Queue.type{type Unbounded = $proxy2.Queue} =
      $proxy2.Queue.asInstanceOf[$proxy2.Queue.type{type Unbounded = $proxy2.Queue}]
    val $proxy3: $proxy1.Unbounded.type{type Unsafe = $proxy2.Queue} =
      $proxy1.Unbounded.asInstanceOf[$proxy1.Unbounded.type{type Unsafe = $proxy2.Queue}]
    val Unbounded$_this:
      $proxy1.Unbounded.type{type Unsafe = $proxy2.Queue} = $proxy3
    val f$proxy1: (outer.Queue.Unbounded => Unit) & ($proxy1.Unbounded => Unit) =
      ((q: outer.Queue.Unbounded) => ???).asInstanceOf[(outer.Queue.Unbounded => Unit) & ($proxy1.Unbounded => Unit)]

    f$proxy1(Unbounded$_this.Unsafe.init())
