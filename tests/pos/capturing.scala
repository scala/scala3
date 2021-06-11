object Test:

  extension [A <: Any retains *] (xs: LazyList[A])
    def lazyMap[B <: Any retains *] (f: A => B retains *): LazyList[B] retains f.type | A | B =
      val x: Int retains f.type | A = ???
      val y = x
      val z: Int retains A retains f.type = y
      ???
