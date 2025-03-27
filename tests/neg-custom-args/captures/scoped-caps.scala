class A
class B
class S extends caps.SharedCapability

def test(io: Object^): Unit =
  val f: (x: A^) -> B^ = ???
  val g: A^ -> B^ = f // error
  val _: (y: A^) -> B^ = f
  val _: (x: A^) -> B^ = g // error
  val _: A^ -> B^ = f // error
  val _: A^ -> B^ = g
  val _: A^ -> B^ = x => g(x)      // should be error, since g is pure, g(x): B^{x} , which does not match B^{fresh}
  val _: (x: A^) -> B^ = x => f(x) // error: existential in B cannot subsume `x` since `x` is not shared

  val h: S -> B^ = ???
  val _: (x: S) -> B^ = h          // error: direct conversion fails
  val _: (x: S) -> B^ = x => h(x)  // but eta expansion succeeds (for SharedCapabilities)

  val j: (x: S) -> B^ = ???
  val _: (x: S) -> B^ = j
  val _: (x: S) -> B^ = x => j(x)
  val _: S -> B^ = j               // error
  val _: S -> B^ = x => j(x)       // should be error

  val g2: A^ => B^ = ???
  val _: A^ => B^ = x => g2(x)  // should be error: g2(x): B^{g2, x}, and the `x` cannot be subsumed by fresh
  val g3: A^ => B^ = ???
  val _: A^{io} => B^ = x => g3(x)  // ok, now g3(x): B^{g3, x}, which is widened to B^{g3, io}
