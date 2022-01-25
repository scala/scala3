object mapOnPair2:
    val p2: (Option[Int], Option[String]) = (1,"foo").map[Option[_]]([T] => (x: T) => Option.apply[T](x))
