trait Bar {
  type A
}

object Test {
  def test0: Unit = {
    val b1: Bar = new Bar {}
    val b2: Bar { type A = Bar#A } = b1 // error
  }
  def test1: Unit = {
    val b1: List[Bar] = List(new Bar {})
    val b2: List[Bar { type A = Bar#A }] = b1 // error
  }
}