class E(f: () => Unit) extends Exception // error

class E2(f: () => Unit) extends Exception:
  self: E^ => // error

object Test extends caps.Mutable:
  var x: Int = 0

  update def foo() = throw E(() => x += 1)

