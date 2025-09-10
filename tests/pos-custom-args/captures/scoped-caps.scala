class A
class B
class S extends caps.SharedCapability

def test(io: Object^): Unit =
  val h2: S -> S = ???
  val _: (x: S) -> S = h2               // direct conversion OK for shared S
  val _: (x: S) -> S = (x: S) => h2(x)  // eta expansion also ok
  val _: (x: S) -> S =
    (x: S) =>
      val y = h2(x) // eta expansion OK
      y

