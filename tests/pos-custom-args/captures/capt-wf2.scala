class C extends caps.SharedCapability

def test(c: C) =
  var x: Any^{c} = ???
  val y: Any^{x} = x  // ok
