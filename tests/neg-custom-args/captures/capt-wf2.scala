@annotation.capability class C

def test(c: C) =
  var x: Any^{c} = ???
  val y: Any^{x} = x  // error
