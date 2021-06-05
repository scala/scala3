object Test:

  extension [A <: Any holds *] (xs: LazyList[A])
    def lazyMap[B <: Any holds *] (f: A => B holds *): LazyList[B] holds f.type | A | B =
      val x: Int holds f.type | A = ???
      val y = x
      val z: Int holds A holds f.type = y
      ???
