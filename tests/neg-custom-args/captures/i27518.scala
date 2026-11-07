import language.experimental.separationChecking
import caps.*
class Watcher(count: => Int):
  def get: Int = count
class Counter extends Mutable:
  private var n = 0
  update def inc(): Int = { n += 1; n }
def test1(): Unit =
  val c = new Counter
  val op: () -> Unit = () => println(c.inc())  // error
  val w: Watcher^{} = new Watcher(c.inc())  // error

// By-name constructor parameters must contribute to the capture set of the
// instance, just like parameters of function type do.
class WatcherF(count: () => Int):
  def get: Int = count()

def test2(): Unit =
  val c = new Counter
  val w1: WatcherF^{} = new WatcherF(() => c.inc())  // error
  val w2: Watcher^{c} = new Watcher(c.inc())  // ok
  val w3 = new Watcher(c.inc())
  val w4: Watcher^{} = w3  // error

def test3(io: Object^): Unit =
  val w1: Watcher^{} = new Watcher(io.hashCode)  // error
  val w2: Watcher^{io} = new Watcher(io.hashCode)  // ok
  val w3: Watcher^{} = new Watcher(42)  // ok
