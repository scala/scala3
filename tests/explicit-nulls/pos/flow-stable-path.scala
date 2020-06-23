// Test stable path is still stable after notNull

class C(val x: Int, val next: C|Null)

def f() = {
  val xs: C|Null = C(1, C(2, null))
  assert(xs != null)
  val a: xs.x.type = xs.x
  assert(xs.next != null)
  val b: xs.next.x.type = xs.next.x
}
