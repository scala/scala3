import caps.fresh
class C extends caps.SharedCapability
def test1(c: C) =
  val a: (x: C) => C^{fresh} = ???
  val b: C => C = ???
  val _: (x: C) => C^{fresh} = b // error
  val _: C => C = a // error
  val _: (x: C) => C^{fresh} = (x: C) => (b(x): C) // OK
  val _: C => C = (x: C) => a(x) // error

def test2(c: C) =
  val a: (x: C) -> C^{fresh} = ???
  val b: C -> C = ???
  val _: (x: C) -> C^{fresh} = b // OK
  val _: C -> C = a // error
  val _: (x: C) -> C^{fresh} = (x: C) => b(x) // ok
  val _: C -> C = (x: C) => a(x) // error

