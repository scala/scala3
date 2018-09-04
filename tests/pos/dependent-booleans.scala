object Test {
  def randomBoolean() = false

  dependent def and(x: Boolean) =
    x && randomBoolean()

  dependent def or(x: Boolean) =
    x || randomBoolean()

  and(true)  : { randomBoolean() }
  and(false) : { false }
  or(true)   : { true }
  or(false)  : { randomBoolean() }


  dependent def and2(x: Boolean) =
    randomBoolean() && x

  dependent def or2(x: Boolean) =
    randomBoolean() || x

  and2(true)  : { randomBoolean() && true }
  and2(false) : { randomBoolean() && false }
  or2(true)   : { randomBoolean() || true }
  or2(false)  : { randomBoolean() || false }
}
