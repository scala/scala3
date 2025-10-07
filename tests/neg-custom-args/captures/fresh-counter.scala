import caps.Mutable
import caps.cap

class Ref extends Mutable:
  var x = 0
  def get: Int = x
  update def put(y: Int): Unit = x = y

class Counter:
  private val count: Ref^ = Ref()
  val incr = () =>
    count.put(count.get + 1)
  val decr = () =>
    count.put(count.get - 1)

def par(p1: () => Unit, p2: () => Unit) = ()

def test() =
  val c = Counter()
  val i = c.incr
  val d = c.decr
  par(i, d) // error: separation failure

