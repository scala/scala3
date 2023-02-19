package fiberRuntime

object util:
  inline val logging = false
  inline def log(inline msg: String) =
    if logging then println(msg)

  private val rand = new java.util.Random

  def sleepABit() =
    Thread.sleep(rand.nextInt(100))

  val threadName = new ThreadLocal[String]
end util
import util.*

/** A delimited contination, which can be invoked with `resume` */
class Suspension:
  private var hasResumed = false
  def resume(): Unit = synchronized:
    hasResumed = true
    notify()
  def suspend(): Unit = synchronized:
    if !hasResumed then
      log(s"suspended ${threadName.get()}")
      wait()

def suspend[T, R](body: Suspension => Unit): Unit =
  sleepABit()
  log(s"suspending ${threadName.get()}")
  val susp = Suspension()
  body(susp)
  sleepABit()
  susp.suspend()

object boundary:
  final class Label[-T]()

  def setName(name: String) =
    log(s"started $name, ${Thread.currentThread.getId()}")
    sleepABit()
    threadName.set(name)

  def apply[T](body: Label[T] ?=> Unit): Unit =
    new Thread:
      override def run() =
        sleepABit()
        try body(using Label[T]())
        finally log(s"finished ${threadName.get()} ${Thread.currentThread.getId()}")
    .start()


