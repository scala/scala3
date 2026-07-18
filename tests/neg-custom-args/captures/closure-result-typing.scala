def test(c: Object^): Unit =
  val x: () -> Object = () => c // error
