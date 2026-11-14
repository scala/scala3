def test(io: IO) =
  val f: () ->{io} String = mk(io)
  val g: () -> String = mk(io) // error
