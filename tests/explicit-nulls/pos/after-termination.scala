class C(val x: Int, val next: C | Null)

def test1(x: String | Null, c: C | Null): Int =
  return 0
  // We know that the following code is unreachable,
  // so we can treat `x`, `c`, and any variable/path non-nullable.
  x.length + c.next.x

def test2(x: String | Null, c: C | Null): Int =
  throw new Exception()
  x.length + c.next.x

def fail(): Nothing = ???

def test3(x: String | Null, c: C | Null): Int =
  fail()
  x.length + c.next.x
