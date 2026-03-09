package test
import caps.Mutable
import caps.{any, SharedCapability, ExclusiveCapability}
import caps.unsafe.untrackedCaptures

class Ref extends Mutable:
  var x = 0
  def get: Int = x
  update def put(y: Int): Unit = x = y

class Counter extends ExclusiveCapability:
  private val count = Ref()
  private val altCount: Ref^ = Ref() // ok

  @untrackedCaptures
  private val altAltCount = Ref() // ok

  val incr = () => // error
    count.put(count.get + 1)
  val decr = () => // error
    count.put(count.get - 1)

trait CounterAPI:
  val count = Ref(): Object^ // error // error
  private def count2 = Ref() // ok

def test() =
  class Counter:
    private val count = Ref() // error
    val incr = () =>
      count.put(count.get + 1)
    val decr = () =>
      count.put(count.get - 1)

class A: // error
  val x: A^{any.only[caps.Control]} = ???
  private val y = ??? : A^{any.only[caps.Control]}  // ok

class B extends caps.Control: // error
  val x: A^ = ???
  private val y = ??? : A^{any.only[caps.Control]}  // ok

class C: // error
  val x: A^{any.only[caps.Control]} = ???
  private val y = ??? : A^ // error

class D extends caps.ExclusiveCapability:
  val x: A^{any.only[caps.Control]} = ???
  private val y = ??? : (() => A^{any.only[caps.Unscoped]})

class E:
  private val y = ??? : A^ // error

