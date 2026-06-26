@main def Test: Unit =
  def check[A](label: String, actual: Vector[A], expected: Vector[A]): Unit =
    assert(actual == expected, s"$label: $actual != $expected")

  check("vector0", Vector(), Vector.empty[Int])
  check("vector1", Vector(1), Vector(1))
  check(
    "vector31",
    Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
      20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30),
    (0 to 30).toVector
  )
  check(
    "vector32",
    Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
      20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31),
    (0 to 31).toVector
  )
  check(
    "vector33",
    Vector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
      20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32),
    (0 to 32).toVector
  )
  check("generic", Vector[Any](1, "a", true), Vector[Any](1, "a", true))
  check("seq", Seq(1, 2, 3).toVector, Vector(1, 2, 3))

  println("ok")
