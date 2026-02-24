def test1(): Unit =
  val t1: (x: () => Unit) -> (y: () ->{x} Unit) -> Unit =
    x => y => ()  // should ok, but error