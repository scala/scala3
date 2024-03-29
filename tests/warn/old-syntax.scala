//> using options  -deprecation

val f = (x: Int) ⇒ x + 1  // warn

val list = for (n ← List(42)) yield n + 1  // warn