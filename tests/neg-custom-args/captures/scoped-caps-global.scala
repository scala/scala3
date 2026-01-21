import caps.fresh
class A
class B
class S extends caps.SharedCapability
import caps.fresh

val io: Object^ = ???

def test(io: Object^): Unit =
  val f: (x: A^) -> B^{fresh} = ???
  val g: A^ -> B^ = f // error

  val _: (y: A^) -> B^{fresh} = f
  val _: (x: A^) -> B^{fresh} = g // error
  val _: A^ -> B^ = f // error
  val _: A^ -> B^ = g
  val _: A^ -> B^ = x => g(x)      // error: g is no longer pure, since it contains the ^ of B
  val _: (x: A^) -> B^{fresh} = x => f(x) // now OK, was error: existential in B cannot subsume `x` since `x` is not shared

  val h: S -> B^ = ???
  val _: (x: S) -> B^{fresh} = h          // error: direct conversion fails
  val _: (x: S) -> B^{fresh} = (x: S) => h(x)  // eta expansion is ok

  val h2: S -> S = ???
  val _: (x: S) -> S^{fresh} = h2               // direct conversion OK for shared S
  val _: (x: S) -> S^{fresh} = (x: S) => h2(x)  // eta conversion is also OK

  val j: (x: S) -> B^{fresh} = ???
  val _: (x: S) -> B^{fresh} = j
  val _: (x: S) -> B^ = x => j(x)  // error
  val _: (x: S) -> B^{fresh} = x => j(x)  // ok
  val _: S -> B^ = j               // error
  val _: S -> B^ = x => j(x)       // error

  val g2: A^ => B^ = ???
  val _: A^ => B^ = x =>
    val y = g2(x)  // now ok since g2(x): B^{g2*}
    val _: B^{g2*} = y
    y
  val g3: A^ => B^ = ???
  val _: A^{io} => B^ = x => g3(x)  // now g3(x): B^{g3*}
