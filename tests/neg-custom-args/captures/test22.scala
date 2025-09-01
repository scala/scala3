class A
class B
class S extends caps.SharedCapability

def test(io: Object^): Unit =
  val h2: S -> S = ???
  val _: (x: S) -> S = h2               // direct conversion OK for shared S
  val _: (x: S) -> S = (x: S) => h2(x)  // error: eta conversion fails since `h2` is now impure (result type S is a capability)
  val _: (x: S) ->{h2} S =
    (x: S) =>
      val y = h2(x) // eta expansion OK
      y

