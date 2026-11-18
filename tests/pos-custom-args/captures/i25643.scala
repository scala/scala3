import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

object Registry extends Stateful:
  var sharedCount = 0
  update class CounterX:
    update def next: Int =
      sharedCount += 1
      sharedCount
  final update class CounterZ(start: Int):
    def next: Int =
      sharedCount += start
      sharedCount
  update def mk(): CounterX^{this} = CounterX()

class Outer extends Stateful:
  var count = 0
  update class Inc:
    def apply(): Unit = count += 1
  update def mk(): Inc^{this} = Inc()

def test(o: Outer^) =
  val i = o.Inc()
  val f: () ->{o} Unit = () => i()
  f()

class Modifiers extends Stateful:
  var x = 0
  sealed update class S:
    def f() = x += 1
  update trait T:
    def f() = x += 1
  abstract update class B
