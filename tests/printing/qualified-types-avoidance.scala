package example

def id(x: Int): {r: Int with r == x} = x.runtimeChecked

def impure(): Int = 42

def test() =
  // Call produces a type with a free ENodeVar (Skolem) — avoidance should
  // weaken the qualifier so the free var doesn't leak into the inferred
  // type of test's result.
  val c = id(impure())

  // Inside a block, free vars in the final expression's type would leak
  // out. Block avoidance should also eliminate them.
  val d =
    val u = impure()
    id(u)

  // Inferred def return type
  def inferred() = id(impure())

  c + d + inferred()
