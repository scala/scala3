object Test {
  def oneOrTwo(x: 1 | 2): 1 | 2 = x // error // error // error // error
  def test: Unit = {
    val foo: 3 | 4 = 1 // error // error
    oneOrTwo(foo)
  }
}
