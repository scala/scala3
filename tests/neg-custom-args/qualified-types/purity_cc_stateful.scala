import language.experimental.captureChecking

class Cell extends caps.Stateful:
  var field: Int = 0
  update def bump(): Unit = field += 1

def testWrite(c: Cell^): Unit =
  def getAndBump(): Int = { c.bump(); c.field }
  val v1: Int with v1 == getAndBump() = getAndBump() // error

// Reads of stateful capabilities are also rejected: the result of `get` can
// change between evaluations.
def testRead(c: Cell^): Unit =
  def get(): Int = c.field
  val v2: Int with v2 == get() = get() // error
