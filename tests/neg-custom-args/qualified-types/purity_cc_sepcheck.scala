import language.experimental.captureChecking
import language.experimental.separationChecking

// Under plain captureChecking, mutation of a class that does not extend
// `Stateful` is untracked, so a predicate calling `getAndBump` would be
// accepted. Separation checking closes this hole: mutable fields must live
// in `Stateful` classes, whose uses are then visible to the purity check.
class Cell:
  var field: Int = 0 // error

def test(c: Cell): Unit =
  def getAndBump(): Int = { c.field += 1; c.field }
  val v1: Int with v1 == getAndBump() = getAndBump()
