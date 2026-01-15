package test
import caps.Mutable
import caps.cap
import caps.unsafe.untrackedCaptures

class Ref extends Mutable:
  var x = 0
  def get: Int = x
  update def put(y: Int): Unit = x = y

class Counter:
  private val count = Ref() // error
  private val altCount: Ref^ = Ref() // ok

  @untrackedCaptures
  private val altAltCount = Ref() // ok

  val incr = () => // error
    count.put(count.get + 1)
  val decr = () => // error
    count.put(count.get - 1)

trait CounterAPI:
  val count = Ref(): Object^ // error
  private def count2 = Ref() // ok

def test() =
  class Counter:
    private val count = Ref() // ok
    val incr = () =>
      count.put(count.get + 1)
    val decr = () =>
      count.put(count.get - 1)

