def test: Unit =
  val x: Int = ???
  val y: Int = ???
  val z: Int = ???
  summon[{v: Int with v == 2 + (x * y * y * z)} <:< {v: Int with v == (x * y * z * y) + 2}]
