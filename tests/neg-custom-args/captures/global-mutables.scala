import caps.Mutable
object Test33 extends Mutable:
  var ctr1 = 0
  var ctr2 = 0
  var ctr3 = 0
  def badIncr() = ctr1 += 1  // error, not an update method
  update def incr() = ctr2 += 1
  val inc1 = () => incr()    // Uses Test33.this via includeCallCaptures
  val _: () -> Unit = inc1   // error
  val inc2 = () => ctr3 += 1 // Uses Test33.this by accessing ctr3
  val _: () -> Unit = inc2   // error

def test2: Unit =
  var ctr = 0
  val inc = () => ctr += 1
  val _: () -> Unit = inc  // error

