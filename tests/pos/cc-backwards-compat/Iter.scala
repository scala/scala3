package p
import language.experimental.captureChecking

class Iter:
  self: Iter^ =>

def test(it: Iter^) =
  val a = A()
  //val b = a.map(it)   // does not work yet
  val c = a.pair(it)
