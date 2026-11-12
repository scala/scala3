import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

object Registry extends Stateful:
  var sharedCount = 0
  class CounterW extends Stateful:
    update def next: Int =
      sharedCount += 1   // error
      sharedCount
  class CounterY:
    def next: Int =
      sharedCount += 1   // error
      sharedCount

class Outer extends Stateful:
  var count = 0
  update class Inc:
    def apply(): Unit = count += 1
  def mk = Inc()  // error

def test(o: Outer^, p: Outer) =
  val _ = p.Inc() // error
  val i = o.Inc()
  val _: () -> Unit = () => i() // error
