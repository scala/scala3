import Macros.*

def powerTest(x: Double): Unit =
  power(x, 0)
  power(x, 1)
  power(x, 5)
  power(x, 10)

def letTest: Unit =
  let(0) { _ + 1 }
  let(0) { _.toString }
  let((4, 'a')) { _.swap }
  let(new Foo) { _.hashCode }

class Foo
