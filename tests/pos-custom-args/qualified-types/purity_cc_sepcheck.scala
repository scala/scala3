import language.experimental.captureChecking
import language.experimental.separationChecking

class Cell extends caps.Mutable:
  var field: Int = 0
  update def bump(): Unit = field += 1

def pure(x: Int): Int = x + 1

// Mutation is fine outside predicates; predicates stay pure by reading a
// stable snapshot of the state instead of the mutable field itself.
def test(c: Cell^): Unit =
  c.bump()
  val snapshot: Int = c.field
  val v: Int with v == pure(snapshot) = pure(snapshot)
