def f(x: Int): Int = ???
def g(x: Int): Int = ???
def f2(x: Int, y: Int): Int = ???
def g2(x: Int, y: Int): Int = ???

def test: Unit =
  val a: Int = ???
  val b: Int = ???
  val c: Int = ???
  val d: Int = ???

  summon[{v: Int with v == 2} <:< {v: Int with v == 2}]
  summon[{v: Int with v == f(a)} <:< {v: Int with v == f(a)}]

  // Equality is reflexive, symmetric and transitive
  summon[{v: Int with v == v} <:< {v: Int with true}]
  summon[{v: Int with a == b} <:< {v: Int with true}]
  summon[{v: Int with v == a} <:< {v: Int with v == a}]
  summon[{v: Int with v == a} <:< {v: Int with a == v}]
  summon[{v: Int with a == b} <:< {v: Int with b == a}]
  summon[{v: Int with v == a && a > 3} <:< {v: Int with v > 3}]
  summon[{v: Int with v == a && a == b} <:< {v: Int with v == b}]
  summon[{v: Int with a == b && b == c} <:< {v: Int with a == c}]
  summon[{v: Int with a == b && c == b} <:< {v: Int with a == c}]
  summon[{v: Int with a == b && c == d && b == d} <:< {v: Int with b == d}]
  summon[{v: Int with a == b && c == d && b == d} <:< {v: Int with a == c}]

  // Equality is congruent over functions
  summon[{v: Int with a == b} <:< {v: Int with f(a) == f(b)}]
  summon[{v: Int with a == b} <:< {v: Int with f(f(a)) == f(f(b))}]
  summon[{v: Int with c == f(a) && d == f(b) && a == b} <:< {v: Int with c == d}]
  // the two first equalities in the premises are just used to test the behavior
  // of the e-graph when `f(a)` and `f(b)` are inserted before `a == b`.
  summon[{v: Int with c == f(a) && d == f(b) && a == b} <:< {v: Int with f(a) == f(b)}]
  summon[{v: Int with c == f(a) && d == f(b) && a == b} <:< {v: Int with f(f(a)) == f(f(b))}]
