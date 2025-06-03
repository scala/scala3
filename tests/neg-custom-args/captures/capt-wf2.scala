class C extends caps.Capability

def test(c: C) =
  var x: Any^{c} = ???
  val y: Any^{x} = x  // error
