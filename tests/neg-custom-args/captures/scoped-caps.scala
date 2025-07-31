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
  val _: A^ -> B^ = x => g(x)      // now ok, was error: since g is pure, g(x): B^{x} , which does not match B^{fresh}
  val _: (x: A^) -> B^ = x => f(x) // error: existential in B cannot subsume `x` since `x` is not shared

  val h: S -> B^ = ???
  val _: (x: S) -> B^ = h          // error: direct conversion fails
  val _: (x: S) -> B^ = (x: S) => h(x)  // error: eta expansion fails

  val h2: S -> S = ???
  val _: (x: S) -> S = h2               // direct conversion OK for shared S
  val _: (x: S) -> S = (x: S) => h2(x)  // eta conversion is also OK

  val j: (x: S) -> B^ = ???
  val _: (x: S) -> B^ = j
  val _: (x: S) -> B^ = x => j(x)
  val _: S -> B^ = j               // error
  val _: S -> B^ = x => j(x)       // error

  val g2: A^ => B^ = ???
  val _: A^ => B^ = x =>
    val y = g2(x)  // now ok since g2(x): B^{g2*}
    val _: B^{g2*} = y
    y
  val g3: A^ => B^ = ???
  val _: A^{io} => B^ = x => g3(x)  // now g3(x): B^{g3*}
