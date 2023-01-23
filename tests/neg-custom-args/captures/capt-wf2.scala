@annotation.capability class C

def test(c: C) =
  var x: {c} Any = ???
  val y: {x} Any = x  // error
