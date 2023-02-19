package fiberRuntime

/** A delimited contination, which can be invoked with `resume` */
class Suspension:
  private var hasResumed = false
  def resume(): Unit = synchronized:
    hasResumed = true
    notify()
  def suspend(): Unit = synchronized:
    if !hasResumed then
      wait()

def suspend[T, R](body: Suspension => Unit): Unit =
  val susp = Suspension()
  body(susp)
  susp.suspend()

object boundary:
  final class Label[-T]()

  def setName(name: String) = ()

  def apply[T](body: Label[T] ?=> Unit): Unit =
    new Thread:
      override def run() =
        body(using Label[T]())
    .start()


