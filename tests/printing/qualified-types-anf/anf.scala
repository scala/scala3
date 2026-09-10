package example

def id(x: Int): {r: Int with r == x} = x.runtimeChecked
def add(x: Int, y: Int): {r: Int with r == x + y} = (x + y).runtimeChecked
def a(): Int = 1
def impure(): Int = 42

def test() =
  // Local val with an escaping qualifier: the skolem must become a sibling
  // `val`, not a block-local one (which avoidance would erase).
  val c = id(impure())

  // Evaluation order preserved: a() lifted before the nested skolem arg.
  val d = add(a(), id(impure()))

  c + d

// Bare (lazily-evaluated) def body: skolem args lift into the body itself.
def inferred() = id(impure())
