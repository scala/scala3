import language.experimental.captureChecking
import caps.fresh

class C extends caps.SharedCapability
class D

def test1 =
  val a: (x: C) -> C^{fresh} = ???
  val b = a

def test2 =
  val a: (x: D^) -> D^{fresh} = ???
  val b = a

