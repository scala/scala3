package p
import language.experimental.captureChecking

class Iter:
  self: Iter^ =>

def test(it: Iter^) =
  val f: Int ->{it} Int = ???
  val a = new A(f)
  val b = a.map(it)   // does not work yet
  val c = a.pair(it)
  val d = a.foo(f)
