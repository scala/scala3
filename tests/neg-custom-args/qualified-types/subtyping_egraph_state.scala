def test: Unit =
  val b: Boolean = ???
  val b2: Boolean = ???
  summon[{u: Unit with b && b2} <:< {u: Unit with b}]
  // Checks that E-Graph state is reset after the implication check: b is no
  // longer true
  summon[{u: Unit with true} <:< {u: Unit with b}] // error
