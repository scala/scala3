import caps.Mutable
import caps.any

class Ref extends Mutable:
  var x = 0
  def get: Int = x
  update def put(y: Int): Unit = x = y

class Counter:
  private val count: Ref^ = Ref()
  val incr = () =>
    count.put(count.get + 1)
  val decr: () ->{this} Unit = () =>
    count.put(count.get - 1)

def par(p1: () => Unit, p2: () => Unit) = ()
def seq(p1: () => Unit, p2: () ->{any, p1} Unit) = ()

def test() =
  val c = Counter()
  val i = c.incr
  val d = c.decr
  par(i, d) // error: separation failure
  seq(i, d)

type Proc = () => Unit

class Pair(val a: Proc, val b: Proc)

def mkCounter(): Pair^ =
  val count = Ref()
  Pair(
    () => count.put(count.get + 1), // error: separation failure
    () => count.put(count.get - 1))

