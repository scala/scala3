import language.experimental.captureChecking

class IO extends caps.SharedCapability:
  def now(): Int = 0

class Cell:
  var field: Int = 0

def test(io: IO): Unit =
  def f(x: Int): Int = { io.now(); x }
  val v1: Int with v1 == f(1) = f(1) // error
  val v2: Int with v2 == io.now() = io.now() // error

def testVar(c: Cell): Unit =
  val v3: Int with v3 == c.field = c.field // error
