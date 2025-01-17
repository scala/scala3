import language.experimental.captureChecking

class C extends caps.Capability
class D

def test1 =
  val a: (x: C) -> C = ???
  val b = a

def test2 =
  val a: (x: D^) -> D^ = ???
  val b = a
