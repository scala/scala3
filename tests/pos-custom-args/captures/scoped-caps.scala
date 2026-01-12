import caps.fresh
class A
class B
class S extends caps.SharedCapability

def test(io: Object^): Unit =
  val h2: S -> S = ???
  val _: (x: S) -> S^{fresh} = h2               // direct conversion OK for shared S
  val _: (x: S) -> S^{fresh} = (x: S) => h2(x)  // eta expansion also ok
  val _: (x: S) -> S^{fresh} =
    (x: S) =>
      val y = h2(x) // eta expansion OK
      y

